"""
================================================================================
PIPELINE INLINE INSPECTION (ILI) RUN ALIGNMENT & FEATURE MATCHER (STREAMLIT V2.1)
================================================================================

PROCESS OVERVIEW:
1. Data Loading: User uploads two ILI tool run CSV files (Base Run and Target Run).
2. Dynamic Structural Mapping: Dropdown menus allow the user to dynamically assign 
   the Distance/Odometer and Depth columns based on mutual fields.
3. Auto-Exclusion Checklist & Warnings: Active mapping fields are automatically 
   filtered out of the feature block. Real-time visual alerts guard against leakage.
4. Clock-to-Degree Normalization: Converts clock positions into degrees.
5. Spatial Window Filtering: Enforces a physical maximum drift cutoff.
6. Shortest Angular Distance KNN: Matches features based on wrap-around arc length.
7. Delta Difference Computation: Calculates depth and odometer differences strictly 
   as (Second Run - First Run).
8. Unified ZIP Exporter: Packs the Master Alignment Sheet, Unmatched Anomalies, 
   Audit Reports, and Verification Plots into a single `.zip` archive.
================================================================================
"""

import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from datetime import datetime
import io
import zipfile

# Set clean, professional page layout
st.set_page_config(page_title="ILI Run Alignment & KNN Matcher", layout="wide")

# --- CUSTOM CSS BUTTON INJECTION ---
st.markdown("""
<style>
    div.stDownloadButton > button {
        background-color: #3B7A57 !important; /* Soft Corporate Sage Green */
        color: #FFFFFF !important;             /* Crisp White Text */
        font-weight: bold !important;
        border-radius: 6px !important;
        border: none !important;
        padding: 0.6rem 1.2rem !important;
        transition: all 0.3s ease-in-out !important;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
    }
    div.stDownloadButton > button:hover {
        background-color: #2C5E43 !important; /* Slightly darker muted green on hover */
        color: #FFFFFF !important;
        box-shadow: 0 4px 8px rgba(0,0,0,0.15) !important;
        transform: translateY(-1px);
    }
    div.stDownloadButton > button:active {
        transform: translateY(1px);
    }
</style>
""", unsafe_allow_html=True)

# Helper Functions
def convert_clock_to_degrees(val):
    if pd.isna(val):
        return np.nan
    val_str = str(val).strip()
    if ':' in val_str:
        parts = val_str.split(':')
        try:
            if len(parts) == 2:
                h, m, s = float(parts[0]), float(parts[1]), 0.0
            elif len(parts) == 3:
                h, m, s = float(parts[0]), float(parts[1]), float(parts[2])
            else:
                return np.nan
            total_hours = h + (m / 60.0) + (s / 3600.0)
            return (total_hours * 30.0) % 360.0
        except ValueError:
            return np.nan
    else:
        try:
            return float(val_str) % 360.0
        except ValueError:
            return np.nan


# ============================================================================
# FEATURE PREPROCESSING LAYER
# ============================================================================

class FeatureProcessor:
    """Encapsulates feature encoding, scaling, and normalization."""
    
    def __init__(self):
        self.scaler = None
        self.category_mappings = {}
        self.angular_std = 1.0
    
    def process_features(self, df1, df2, selected_features, angular_col=None):
        df1_proc = df1.copy()
        df2_proc = df2.copy()
        
        # Step 1: Process angular feature
        linear_features = selected_features.copy()
        if angular_col and angular_col in linear_features:
            linear_features.remove(angular_col)
            df1_proc[angular_col] = df1_proc[angular_col].apply(convert_clock_to_degrees)
            df2_proc[angular_col] = df2_proc[angular_col].apply(convert_clock_to_degrees)
            
            combined_angles = pd.concat([df1_proc[angular_col], df2_proc[angular_col]])
            median_angle = combined_angles.median()
            fill_val = median_angle if not pd.isna(median_angle) else 0.0
            df1_proc[angular_col] = df1_proc[angular_col].fillna(fill_val)
            df2_proc[angular_col] = df2_proc[angular_col].fillna(fill_val)
            
            self.angular_std = combined_angles.std()
            if self.angular_std == 0 or np.isnan(self.angular_std):
                self.angular_std = 1.0
        
        # Step 2: Encode categorical features and fill numeric NaNs
        encoded_features = []
        for col in linear_features:
            if not pd.api.types.is_numeric_dtype(df1_proc[col]) or \
               not pd.api.types.is_numeric_dtype(df2_proc[col]):
                s1 = df1_proc[col].astype(str).str.strip()
                s2 = df2_proc[col].astype(str).str.strip()
                combined = pd.concat([s1, s2])
                categories = sorted(combined.unique())
                self.category_mappings[col] = categories
                
                enc_name = f"{col}_encoded"
                df1_proc[enc_name] = pd.Categorical(s1, categories=categories).codes
                df2_proc[enc_name] = pd.Categorical(s2, categories=categories).codes
                encoded_features.append(enc_name)
            else:
                for df in [df1_proc, df2_proc]:
                    if df[col].isna().all():
                        df[col] = 0
                    else:
                        df[col] = df[col].fillna(df[col].median())
                encoded_features.append(col)
        
        # Step 3: Standardize all linear/encoded features
        if encoded_features:
            combined = pd.concat([df1_proc[encoded_features], df2_proc[encoded_features]], axis=0)
            self.scaler = StandardScaler()
            self.scaler.fit(combined)
            X1_scaled = self.scaler.transform(df1_proc[encoded_features])
            X2_scaled = self.scaler.transform(df2_proc[encoded_features])
        else:
            X1_scaled = np.array([]).reshape(len(df1_proc), 0)
            X2_scaled = np.array([]).reshape(len(df2_proc), 0)
        
        return df1_proc, df2_proc, X1_scaled, X2_scaled, encoded_features


# ============================================================================
# KNN MATCHING LAYER
# ============================================================================

class KNNMatcher:
    """Vectorized KNN matching with spatial constraints."""
    
    def __init__(self, max_drift, angular_col=None, angular_std=1.0):
        self.max_drift = max_drift
        self.angular_col = angular_col
        self.angular_std = angular_std
    
    def match(self, df1_proc, df2_proc, X1_scaled, X2_scaled, distance_col, encoded_features):
        matched_indices = []
        distances = []
        match_stats = {
            "total_base_rows": len(df1_proc),
            "matched_rows": 0,
            "unmatched_rows": 0,
            "distance_stats": {}
        }
        
        for i, row in df1_proc.iterrows():
            w_val = row[distance_col]
            
            mask = (df2_proc[distance_col] >= w_val - self.max_drift) & \
                   (df2_proc[distance_col] <= w_val + self.max_drift)
            candidate_indices = np.where(mask.values)[0]
            
            if len(candidate_indices) == 0:
                matched_indices.append(None)
                distances.append(np.nan)
                match_stats["unmatched_rows"] += 1
            else:
                if X1_scaled.shape[1] > 0:
                    linear_dist_sq = np.sum(
                        (X2_scaled[candidate_indices, :] - X1_scaled[i, :]) ** 2,
                        axis=1
                    )
                else:
                    linear_dist_sq = np.zeros(len(candidate_indices))
                
                if self.angular_col:
                    candidate_angles = df2_proc.iloc[candidate_indices][self.angular_col].values
                    row_angle = row[self.angular_col]
                    shortest_ang = np.minimum(
                        np.abs(candidate_angles - row_angle),
                        360.0 - np.abs(candidate_angles - row_angle)
                    )
                    ang_dist_sq = (shortest_ang / self.angular_std) ** 2
                else:
                    ang_dist_sq = np.zeros(len(candidate_indices))
                
                total_dist = np.sqrt(linear_dist_sq + ang_dist_sq)
                best_idx_in_candidates = np.argmin(total_dist)
                best_global_idx = candidate_indices[best_idx_in_candidates]
                best_distance = total_dist[best_idx_in_candidates]
                
                matched_indices.append(best_global_idx)
                distances.append(best_distance)
                match_stats["matched_rows"] += 1
        
        valid_distances = [d for d in distances if not np.isnan(d)]
        if valid_distances:
            match_stats["distance_stats"] = {
                "min": float(np.min(valid_distances)),
                "max": float(np.max(valid_distances)),
                "mean": float(np.mean(valid_distances)),
                "median": float(np.median(valid_distances)),
                "std": float(np.std(valid_distances))
            }
        
        return matched_indices, distances, match_stats


# ============================================================================
# REPORTING LAYER
# ============================================================================

def create_quality_report(match_stats, distance_col, depth_col, df_final):
    lines = [
        "=" * 80,
        "KNN MATCHING QUALITY REPORT",
        "=" * 80,
        f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
        "",
        "MATCH STATISTICS:",
        f"  Total Base Rows:     {match_stats['total_base_rows']}",
        f"  Successfully Matched: {match_stats['matched_rows']}",
        f"  Unmatched (Anomalies): {match_stats['unmatched_rows']}",
        f"  Match Rate:          {100 * match_stats['matched_rows'] / match_stats['total_base_rows']:.1f}%",
        ""
    ]
    
    if match_stats["distance_stats"]:
        stats = match_stats["distance_stats"]
        lines.extend([
            "KNN DISTANCE DISTRIBUTION:",
            f"  Minimum:  {stats['min']:.6f}",
            f"  Maximum:  {stats['max']:.6f}",
            f"  Mean:     {stats['mean']:.6f}",
            f"  Median:   {stats['median']:.6f}",
            f"  Std Dev:  {stats['std']:.6f}",
            ""
        ])
    
    depth_diff_col = f"{depth_col}_difference"
    dist_diff_col = f"{distance_col}_difference"
    
    if depth_diff_col in df_final.columns:
        valid_depth_diffs = df_final[depth_diff_col].dropna()
        if len(valid_depth_diffs) > 0:
            lines.extend([
                f"DEPTH CHANGE ({depth_col} [Second Run - First Run]):",
                f"  Mean Difference:     {valid_depth_diffs.mean():.6f}",
                f"  Median Difference:   {valid_depth_diffs.median():.6f}",
                f"  Std Dev:             {valid_depth_diffs.std():.6f}",
                f"  Min:                 {valid_depth_diffs.min():.6f}",
                f"  Max:                 {valid_depth_diffs.max():.6f}",
                ""
            ])
    
    if dist_diff_col in df_final.columns:
        valid_dist_diffs = df_final[dist_diff_col].dropna()
        if len(valid_dist_diffs) > 0:
            lines.extend([
                f"ODOMETER DRIFT ({distance_col} [Second Run - First Run]):",
                f"  Mean Difference:     {valid_dist_diffs.mean():.6f}",
                f"  Median Difference:   {valid_dist_diffs.median():.6f}",
                f"  Std Dev:             {valid_dist_diffs.std():.6f}",
                f"  Min:                 {valid_dist_diffs.min():.6f}",
                f"  Max:                 {valid_dist_diffs.max():.6f}",
                ""
            ])
    
    lines.extend([
        "=" * 80
    ])
    return "\n".join(lines)


def create_unmatched_summary(df_unmatched, distance_col):
    lines = [
        "=" * 80,
        "UNMATCHED ANOMALIES SUMMARY",
        "=" * 80,
        f"Total Unmatched: {len(df_unmatched)}",
        ""
    ]
    
    if len(df_unmatched) > 0 and distance_col in df_unmatched.columns:
        valid_dists = df_unmatched[distance_col].dropna()
        if len(valid_dists) > 0:
            lines.extend([
                f"Odometer/Distance Range of Unmatched Features:",
                f"  Min:  {valid_dists.min():.2f}",
                f"  Max:  {valid_dists.max():.2f}",
                f"  Mean: {valid_dists.mean():.2f}",
                ""
            ])
    
    lines.extend([
        "These rows could not be matched within the spatial drift constraint.",
        "Review the 'unmatched_anomalies' CSV for details.",
        "=" * 80
    ])
    return "\n".join(lines)


# ============================================================================
# UI LAYER
# ============================================================================

st.markdown("<h2 style='color: #2B5B84; margin-bottom: 0px;'>ILI Run Alignment & Feature Matcher</h2>", unsafe_allow_html=True)
st.markdown("<p style='color: #555; font-style: italic;'>Data Alignment Suite using Spatial Constraint & Shortest Angular Distance KNN</p>", unsafe_allow_html=True)

with st.expander("📖 System Overview, Operational Prerequisites & Key Assumptions", expanded=True):
    st.markdown("""
    ### 🔬 Engineering Assumptions & Prerequisites
    * **Anomaly Classification Filter:** The input datasets **MUST only include metal loss anomalies**. Other feature types—such as manufacturing defects, geometric dents, cracks, mill anomalies, or component markers—must be strictly filtered out of the CSV files before loading them into this app.
    * **Identical Variable Naming:** All fields intended for matching, along with the critical Distance/Odometer and Depth columns, must be named identically (case-sensitive) between both input files.
    * **Data Value Consistency:** Data formats must match exactly between both inspection runs. For example, if one file logs an internal surface anomaly as `"Int"` and the other logs it as `"Internal"`, you must edit one of the files to achieve complete consistency before executing the runner.
    
    ### 🛡️ Critical Field Exclusion Rules
    * **Odometer & Joint Numbers:** Identifiers like `joint_no` or `Item No.` must **NOT** be used as matching features. Joint numbering systems change frequently between runs due to newly added pipeline features, excavations, or odometer slippage.
    * **Depth Fields:** Do **NOT** select `depth` as a matching feature under any circumstances. The direct objective of this pipeline analysis is to calculate and infer true depth change over time. Using depth to find the nearest neighbor causes total data leakage and invalidates the model.
    
    ### ⚡ Performance Notes
    * Features are processed with **vectorized distance matrix computation** for optimal computational efficiency.
    * Differences for depth and odometer/wheel values are calculated strictly as **Second Run - First Run**.
    * Match quality statistics are included in the output report package.
    """)

st.divider()

# --- SIDEBAR CONFIGURATION ---
st.sidebar.markdown("### ⚙️ Processing Parameters")
pipeline_name_input = st.sidebar.text_input("Pipeline Name / System ID", placeholder="e.g., Line 42 Main")
max_drift = st.sidebar.slider("Maximum Allowable Odometer Drift", min_value=50.0, max_value=1000.0, value=200.0, step=25.0)

if pipeline_name_input:
    clean_pipe_name = str(pipeline_name_input).replace(" ", "").strip()
else:
    clean_pipe_name = "PipelineAlignment"

# --- STEP 1: FILE SELECTION ---
st.markdown("#### 1️⃣ Upload Inspection Data Files (CSV)")
col1, col2 = st.columns(2)

with col1:
    file_base = st.file_uploader("Upload Base Run Dataset [1st Run] (e.g., 2005)", type=["csv"], key="base_file")
with col2:
    file_target = st.file_uploader("Upload Target Run Dataset [2nd Run] (e.g., 2011)", type=["csv"], key="target_file")

if file_base and file_target:
    try:
        cols1 = pd.read_csv(file_base, nrows=0).columns.tolist()
        cols2 = pd.read_csv(file_target, nrows=0).columns.tolist()
        mutual_cols = sorted(list(set(cols1).intersection(set(cols2))))

        if not mutual_cols:
            st.error("❌ Architectural Error: No overlapping column names found between these two files.")
        else:
            # --- STEP 2: DYNAMIC COLUMN MAPPING ---
            st.markdown("#### 2️⃣ Map Critical Pipeline Columns")
            m_col1, m_col2 = st.columns(2)
            
            dist_default = next((c for c in mutual_cols if c.lower() in ['wheel', 'chainage', 'distance', 'odometer', 'dist']), mutual_cols[0])
            depth_default = next((c for c in mutual_cols if c.lower() in ['depth', 'dpth']), mutual_cols[min(1, len(mutual_cols)-1)])
            
            with m_col1:
                distance_col = st.selectbox("Distance / Odometer Key:", options=mutual_cols, index=mutual_cols.index(dist_default))
            with m_col2:
                depth_col = st.selectbox("Depth Inference Key:", options=mutual_cols, index=mutual_cols.index(depth_default))
                
            if distance_col == depth_col:
                st.error("⚠️ Mapping Conflict: Distance Column and Depth Column cannot point to the exact same tracking attribute.")
            
            # --- STEP 3: FEATURE CHECKLIST ---
            st.markdown("#### 3️⃣ Select Fields for KNN Distance Matching")
            
            exclude_keywords = ['joint_no', 'item no.']
            default_features = [c for c in mutual_cols if c.lower() not in exclude_keywords and c != distance_col and c != depth_col]
            
            selected_knn_features = st.multiselect(
                "Choose Morphological Fields to Determine Matrix Proximity (Excludes Mapped Columns Automatically):",
                options=mutual_cols,
                default=default_features
            )
            
            leakage_conflicts = [c for c in selected_knn_features if c == distance_col or c == depth_col]
            if leakage_conflicts:
                st.warning(f"⚠️ **Data Leakage Alert:** You have explicitly included `{', '.join(leakage_conflicts)}` in your matching features. Including indexing attributes or your target variable violates machine learning validation principles.")
            
            # --- STEP 4: CORE TRIGGER ---
            st.markdown("---")
            if st.button("🚀 Run Spatial Alignment & KNN Matching Matrix", type="primary", disabled=(distance_col == depth_col or len(selected_knn_features) == 0)):
                
                file_base.seek(0)
                file_target.seek(0)
                
                df1 = pd.read_csv(file_base)
                df2 = pd.read_csv(file_target)
                
                with st.spinner("Executing dynamic structural alignment loop..."):
                    df1_proc, df2_proc = df1.copy(), df2.copy()
                    
                    # Process Angles
                    angular_col = next((col for col in selected_knn_features if col.lower() in ['degrees', 'orientation', 'clock']), None)
                    if angular_col:
                        df1_proc[angular_col] = df1_proc[angular_col].apply(convert_clock_to_degrees)
                        df2_proc[angular_col] = df2_proc[angular_col].apply(convert_clock_to_degrees)
                        combined_angles = pd.concat([df1_proc[angular_col], df2_proc[angular_col]])
                        median_angle = combined_angles.median()
                        fill_val = median_angle if not pd.isna(median_angle) else 0.0
                        df1_proc[angular_col] = df1_proc[angular_col].fillna(fill_val)
                        df2_proc[angular_col] = df2_proc[angular_col].fillna(fill_val)
                        angular_std = combined_angles.std()
                        if angular_std == 0 or np.isnan(angular_std):
                            angular_std = 1.0
                    else:
                        angular_std = 1.0

                    # Process Linear and Mapped Strings
                    linear_features = [col for col in selected_knn_features if col != angular_col]
                    encoded_linear_features = []
                    
                    for col in linear_features:
                        if not pd.api.types.is_numeric_dtype(df1_proc[col]) or not pd.api.types.is_numeric_dtype(df2_proc[col]):
                            s1 = df1_proc[col].astype(str).str.strip()
                            s2 = df2_proc[col].astype(str).str.strip()
                            combined_categories = pd.concat([s1, s2]).astype('category').cat.categories
                            
                            enc_col_name = f"{col}_encoded"
                            df1_proc[enc_col_name] = pd.Categorical(s1, categories=combined_categories).codes
                            df2_proc[enc_col_name] = pd.Categorical(s2, categories=combined_categories).codes
                            encoded_linear_features.append(enc_col_name)
                        else:
                            df1_proc[col] = df1_proc[col].fillna(df1_proc[col].median() if not df1_proc[col].isna().all() else 0)
                            df2_proc[col] = df2_proc[col].fillna(df2_proc[col].median() if not df2_proc[col].isna().all() else 0)
                            encoded_linear_features.append(col)
                    
                    # Standardize linear dimensions
                    if encoded_linear_features:
                        scaler = StandardScaler()
                        scaler.fit(pd.concat([df1_proc[encoded_linear_features], df2_proc[encoded_linear_features]], axis=0))
                        X1_linear_scaled = scaler.transform(df1_proc[encoded_linear_features])
                        X2_linear_scaled = scaler.transform(df2_proc[encoded_linear_features])
                    else:
                        X1_linear_scaled = np.array([]).reshape(len(df1_proc), 0)
                        X2_linear_scaled = np.array([]).reshape(len(df2_proc), 0)
                    
                    df1_proc[distance_col] = df1_proc[distance_col].fillna(0)
                    df2_proc[distance_col] = df2_proc[distance_col].fillna(0)
                    
                    # Compute spatial neighbor alignment using custom layer logic
                    matcher = KNNMatcher(max_drift=max_drift, angular_col=angular_col, angular_std=angular_std)
                    matched_indices, distances, match_stats = matcher.match(
                        df1_proc, df2_proc, X1_linear_scaled, X2_linear_scaled, distance_col, encoded_linear_features
                    )
                    
                    # Build Output Matrix Structure
                    matched_rows = [df2.loc[idx] if idx is not None else pd.Series(np.nan, index=df2.columns) for idx in matched_indices]
                    df_matched_2 = pd.DataFrame(matched_rows).reset_index(drop=True)
                    df_matched_2.columns = [f"{col}_secondRun" for col in df2.columns]
                    
                    df_final = pd.concat([df1.reset_index(drop=True), df_matched_2], axis=1)
                    df_final['knn_distance'] = distances
                    
                    # DIRECT DIFFERENCE CALCULATION: Second Run minus First Run
                    df_final[f'{depth_col}_difference'] = df_final[f'{depth_col}_secondRun'] - df_final[depth_col]
                    df_final[f'{distance_col}_difference'] = df_final[f'{distance_col}_secondRun'] - df_final[distance_col]
                    
                    valid_mask = [idx is not None for idx in matched_indices]
                    df_unmatched = df1[~np.array(valid_mask)].copy()
                    total_unmatched = len(df_unmatched)
                    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

                st.success("🎉 Feature matching matrix generated successfully!")
                
                # Metric Cards Display
                m1, m2, m3 = st.columns(3)
                m1.metric("Total Baseline Rows", len(df1))
                m2.metric("Successfully Paired", sum(valid_mask))
                m3.metric("Pruned Spurious Features", total_unmatched)
                
                # --- RESULTS INTERACTIVE PLOT MATRIX ---
                st.markdown("### 📊 Alignment Diagnostic Dashboard")
                sns.set_theme(style="whitegrid")
                p_col1, p_col2, p_col3 = st.columns(3)
                
                buf1, buf2, buf3 = io.BytesIO(), io.BytesIO(), io.BytesIO()
                
                with p_col1:
                    fig1, ax1 = plt.subplots(figsize=(5, 4))
                    sns.histplot(data=df_final.dropna(subset=[f'{depth_col}_difference']), x=f'{depth_col}_difference', color='#4CAF50', kde=True, bins=30, ax=ax1)
                    ax1.axvline(0, color='red', linestyle='--')
                    ax1.set_title(f'Depth Change ({depth_col} [2nd - 1st])', fontsize=10, fontweight='bold')
                    st.pyplot(fig1)
                    fig1.savefig(buf1, format="png", dpi=300)
                    buf1.seek(0)
                    
                with p_col2:
                    fig2, ax2 = plt.subplots(figsize=(5, 4))
                    sns.scatterplot(x=depth_col, y=f'{depth_col}_secondRun', data=df_final.dropna(subset=[f'{depth_col}_secondRun']), alpha=0.6, color='#2E7D32', ax=ax2)
                    max_val = max(df_final[depth_col].dropna().max(), df_final[f'{depth_col}_secondRun'].dropna().max())
                    ax2.plot([0, max_val], [0, max_val], color='red', linestyle='--')
                    ax2.set_title('Depth Measurement Parity Line', fontsize=10, fontweight='bold')
                    st.pyplot(fig2)
                    fig2.savefig(buf2, format="png", dpi=300)
                    buf2.seek(0)
                    
                with p_col3:
                    fig3, ax3 = plt.subplots(figsize=(5, 4))
                    sns.histplot(data=df_final.dropna(subset=[f'{distance_col}_difference']), x=f'{distance_col}_difference', bins=30, color='#1976D2', kde=True, ax=ax3)
                    ax3.axvline(0, color='black', linestyle=':')
                    ax3.set_title(f'Odometer Drift ({distance_col} [2nd - 1st])', fontsize=10, fontweight='bold')
                    st.pyplot(fig3)
                    fig3.savefig(buf3, format="png", dpi=300)
                    buf3.seek(0)

                # --- UNIFIED IN-MEMORY ZIP ARCHIVER PACKAGE ---
                st.markdown("### 📥 Reports & Deliverables Package")
                
                text_quality_report = create_quality_report(match_stats, distance_col, depth_col, df_final)
                text_unmatched_summary = create_unmatched_summary(df_unmatched, distance_col)
                
                zip_buffer = io.BytesIO()
                
                with zipfile.ZipFile(zip_buffer, "w", zipfile.ZIP_DEFLATED) as zip_file:
                    # 1. Master Aligned Dataset
                    csv_master = df_final.to_csv(index=False).encode('utf-8')
                    zip_file.writestr(f"{clean_pipe_name}_matched_spatial_depth_change_{timestamp}.csv", csv_master)
                    
                    # 2. Pruned Outlier Anomalies Dataset
                    csv_unmatched = df_unmatched.to_csv(index=False).encode('utf-8')
                    zip_file.writestr(f"{clean_pipe_name}_unmatched_anomalies_{timestamp}.csv", csv_unmatched)
                    
                    # 3. Graphical Plots
                    zip_file.writestr(f"{clean_pipe_name}_depth_change_distribution_{timestamp}.png", buf1.getvalue())
                    zip_file.writestr(f"{clean_pipe_name}_depth_comparison_scatter_{timestamp}.png", buf2.getvalue())
                    zip_file.writestr(f"{clean_pipe_name}_odometer_alignment_drift_{timestamp}.png", buf3.getvalue())
                    
                    # 4. Plain Text Audit Reports
                    zip_file.writestr(f"{clean_pipe_name}_quality_audit_report_{timestamp}.txt", text_quality_report)
                    zip_file.writestr(f"{clean_pipe_name}_unmatched_summary_{timestamp}.txt", text_unmatched_summary)
                
                zip_buffer.seek(0)
                
                st.download_button(
                    label="📥 Download All Aligned Datasets & Graphical Plots (.zip)",
                    data=zip_buffer.getvalue(),
                    file_name=f"{clean_pipe_name}_{timestamp}.zip",
                    mime="application/zip",
                    use_container_width=True
                )

    except Exception as e:
        st.error(f"Execution Stopped due to data fault: {str(e)}")
else:
    st.info("💡 Pipeline Status: Waiting for user to upload baseline and comparison inspection CSV datasets above.")