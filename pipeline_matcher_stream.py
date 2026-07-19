"""
================================================================================
PIPELINE INLINE INSPECTION (ILI) RUN ALIGNMENT & FEATURE MATCHER (STREAMLIT V1.0)
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
7. Output Archiving: Safely exports data sheets and dashboard visualizations with 
   unique `_YYYYMMDD_HHMMSS` timestamp signatures.

CRITICAL ASSUMPTIONS & PREREQUISITES:
- Anomaly Classification Filter: The input datasets MUST only include metal loss 
  anomalies. Other feature types—such as manufacturing defects, geometric dents, 
  cracks, or component markers—must be pre-filtered out.
- Identical Variable Naming: All fields intended for matching, along with the critical 
  Distance/Odometer and Depth columns, must be named identically (case-sensitive) 
  between both input files.
- Data Type & Text Value Consistency: Data formats must match exactly between both 
  inspection runs. For example, if one file logs an internal anomaly as "Int" and 
  the other logs it as "Internal", standardizing them first is required.
================================================================================
"""

import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from datetime import datetime
import os

# Set clean, professional page layout
st.set_page_config(page_title="ILI Run Alignment & KNN Matcher", layout="wide")

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

# --- APP HEADER BANNER ---
st.markdown("<h2 style='color: #2B5B84; margin-bottom: 0px;'>ILI Run Alignment & Feature Matcher</h2>", unsafe_allow_html=True)
st.markdown("<p style='color: #555; font-style: italic;'>Data Alignment Suite using Spatial Constraint & Shortest Angular Distance KNN</p>", unsafe_allow_html=True)
st.hr()

# --- SIDEBAR CONFIGURATION ---
st.sidebar.markdown("### ⚙️ Processing Parameters")
max_drift = st.sidebar.slider("Maximum Allowable Odometer Drift", min_value=50.0, max_value=1000.0, value=200.0, step=25.0)
local_save_path = st.sidebar.text_input("Local Target Export Path (Optional)", value="./output")

# --- STEP 1: FILE SELECTION ---
st.markdown("#### 1️⃣ Upload Inspection Data Files (CSV)")
col1, col2 = st.columns(2)

with col1:
    file_base = st.file_uploader("Upload Base Run Dataset (e.g., 2005)", type=["csv"], key="base_file")
with col2:
    file_target = st.file_uploader("Upload Target Run Dataset (e.g., 2011)", type=["csv"], key="target_file")

if file_base and file_target:
    try:
        # Read column headers efficiently
        cols1 = pd.read_csv(file_base, nrows=0).columns.tolist()
        cols2 = pd.read_csv(file_target, nrows=0).columns.tolist()
        mutual_cols = sorted(list(set(cols1).intersection(set(cols2))))

        if not mutual_cols:
            st.error("❌ Architectural Error: No overlapping column names found between these two files.")
        else:
            # --- STEP 2: DYNAMIC COLUMN MAPPING ---
            st.markdown("#### 2️⃣ Map Critical Pipeline Columns")
            m_col1, m_col2 = st.columns(2)
            
            # Intelligently assign best match defaults
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
            
            # Dynamically exclude metrics mapped in Section 2 + index keys from the selection pool
            exclude_keywords = ['joint_no', 'item no.']
            default_features = [c for c in mutual_cols if c.lower() not in exclude_keywords and c != distance_col and c != depth_col]
            
            selected_knn_features = st.multiselect(
                "Choose Morphological Fields to Determine Matrix Proximity (Excludes Mapped Columns Automatically):",
                options=mutual_cols,
                default=default_features
            )
            
            # Real-time Warning Guardrail
            leakage_conflicts = [c for c in selected_knn_features if c == distance_col or c == depth_col]
            if leakage_conflicts:
                st.warning(f"⚠️ **Data Leakage Alert:** You have explicitly included `{', '.join(leakage_conflicts)}` in your matching features. Including indexing attributes or your target variable violates machine learning validation principles.")
            
            # --- STEP 4: CORE TRIGGER ---
            st.markdown("---")
            if st.button("🚀 Run Spatial Alignment & KNN Matching Matrix", type="primary", disabled=(distance_col == depth_col or len(selected_knn_features) == 0)):
                
                # Reset file streams back to step 0 pointer read positions
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
                        median_angle = pd.concat([df1_proc[angular_col], df2_proc[angular_col]]).median()
                        df1_proc[angular_col] = df1_proc[angular_col].fillna(median_angle if not pd.isna(median_angle) else 0.0)
                        df2_proc[angular_col] = df2_proc[angular_col].fillna(median_angle if not pd.isna(median_angle) else 0.0)
                        angular_std = pd.concat([df1_proc[angular_col], df2_proc[angular_col]]).std()
                        if angular_std == 0 or np.isnan(angular_std):
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
                    
                    df1_proc[distance_col] = df1_proc[distance_col].fillna(0)
                    df2_proc[distance_col] = df2_proc[distance_col].fillna(0)
                    
                    # Core Spatial Cutoff Loop
                    matched_indices, distances = [], []
                    for i, row in df1_proc.iterrows():
                        w_val = row[distance_col]
                        candidates = df2_proc[(df2_proc[distance_col] >= w_val - max_drift) & 
                                              (df2_proc[distance_col] <= w_val + max_drift)]
                        
                        if len(candidates) == 0:
                            matched_indices.append(None)
                            distances.append(np.nan)
                        else:
                            linear_dist_sq = np.sum((X2_linear_scaled[candidates.index] - X1_linear_scaled[i]) ** 2, axis=1) if encoded_linear_features else np.zeros(len(candidates))
                            if angular_col:
                                shortest_ang_diff = np.minimum(np.abs(candidates[angular_col].values - row[angular_col]), 360.0 - np.abs(candidates[angular_col].values - row[angular_col]))
                                ang_dist_sq = (shortest_ang_diff / angular_std) ** 2
                            else:
                                ang_dist_sq = np.zeros(len(candidates))
                                
                            total_dists = np.sqrt(linear_dist_sq + ang_dist_sq)
                            best_match_idx = np.argmin(total_dists)
                            matched_indices.append(candidates.index[best_match_idx])
                            distances.append(total_dists[best_match_idx])
                    
                    # Build Output Matrix Structure
                    matched_rows = [df2.loc[idx] if idx is not None else pd.Series(np.nan, index=df2.columns) for idx in matched_indices]
                    df_matched_2 = pd.DataFrame(matched_rows).reset_index(drop=True)
                    df_matched_2.columns = [f"{col}_secondRun" for col in df2.columns]
                    
                    df_final = pd.concat([df1.reset_index(drop=True), df_matched_2], axis=1)
                    df_final['knn_distance'] = distances
                    df_final[f'{depth_col}_difference'] = df_final[depth_col] - df_final[f'{depth_col}_secondRun']
                    df_final[f'{distance_col}_difference'] = df_final[distance_col] - df_final[f'{distance_col}_secondRun']
                    
                    valid_mask = [idx is not None for idx in matched_indices]
                    df_unmatched = df1[~np.array(valid_mask)].copy()
                    total_unmatched = len(df_unmatched)
                    
                    # Archive to Local Path if defined and path exists
                    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                    if local_save_path:
                        try:
                            os.makedirs(local_save_path, exist_ok=True)
                            df_final.to_csv(os.path.join(local_save_path, f'ML2_matched_spatial_depth_change_{timestamp}.csv'), index=False)
                            df_unmatched.to_csv(os.path.join(local_save_path, f'ML2_unmatched_anomalies_{timestamp}.csv'), index=False)
                            st.info(f"💾 Local copies safely written to target directory: `{local_save_path}`")
                        except Exception as path_err:
                            st.warning(f"Could not write directly to local filesystem path: {str(path_err)}")

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
                
                with p_col1:
                    fig1, ax1 = plt.subplots(figsize=(5, 4))
                    sns.histplot(data=df_final.dropna(subset=[f'{depth_col}_difference']), x=f'{depth_col}_difference', color='#4CAF50', kde=True, bins=30, ax=ax1)
                    ax1.axvline(0, color='red', linestyle='--')
                    ax1.set_title(f'Depth Change Profile ({depth_col})', fontsize=10, fontweight='bold')
                    st.pyplot(fig1)
                    
                with p_col2:
                    fig2, ax2 = plt.subplots(figsize=(5, 4))
                    sns.scatterplot(x=depth_col, y=f'{depth_col}_secondRun', data=df_final.dropna(subset=[f'{depth_col}_secondRun']), alpha=0.6, color='#2E7D32', ax=ax2)
                    max_val = max(df_final[depth_col].dropna().max(), df_final[f'{depth_col}_secondRun'].dropna().max())
                    ax2.plot([0, max_val], [0, max_val], color='red', linestyle='--')
                    ax2.set_title('Depth Measurement Parity Line', fontsize=10, fontweight='bold')
                    st.pyplot(fig2)
                    
                with p_col3:
                    fig3, ax3 = plt.subplots(figsize=(5, 4))
                    sns.histplot(data=df_final.dropna(subset=[f'{distance_col}_difference']), x=f'{distance_col}_difference', bins=30, color='#1976D2', kde=True, ax=ax3)
                    ax3.axvline(0, color='black', linestyle=':')
                    ax3.set_title('Odometer Spatial Drift Validation', fontsize=10, fontweight='bold')
                    st.pyplot(fig3)

                # --- INSTANT WEB DOWNLOAD DOWNLOAD BUTTONS ---
                st.markdown("### 📥 Web File Exporter Packages")
                csv_master = df_final.to_csv(index=False).encode('utf-8')
                csv_unmatched = df_unmatched.to_csv(index=False).encode('utf-8')
                
                d1, d2 = st.columns(2)
                with d1:
                    st.download_button(
                        label="📥 Download Aligned Master Table (CSV)",
                        data=csv_master,
                        file_name=f"ML2_matched_spatial_depth_change_{timestamp}.csv",
                        mime="text/csv"
                    )
                with d2:
                    st.download_button(
                        label="📥 Download Unmatched Anomalies Sub-Scoped Table (CSV)",
                        data=csv_unmatched,
                        file_name=f"ML2_unmatched_anomalies_{timestamp}.csv",
                        mime="text/csv"
                    )

    except Exception as e:
        st.error(f"Execution Stopped due to data fault: {str(e)}")
else:
    st.info("💡 Pipeline Status: Waiting for user to upload baseline and comparison inspection CSV datasets above.")