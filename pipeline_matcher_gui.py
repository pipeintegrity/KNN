"""
================================================================================
PIPELINE INLINE INSPECTION (ILI) RUN ALIGNMENT & FEATURE MATCHER (V2)
================================================================================

PROCESS OVERVIEW:
1. Data Loading: User selects two ILI tool run CSV files via GUI.
2. Feature Discovery: Identifies overlapping fields and provides selection checkboxes.
3. Clock-to-Degree Normalization: Automatically converts angular data presented as 
   clock positions ('hh:mm' or 'hh:mm:ss') into standard geometric degrees.
4. Feature Engineering & Scaling: Non-numeric columns (e.g., 'INT_EXT') are mapped to 
   unified category codes. Numeric fields are scaled using a StandardScaler.
5. Spatial Window Filtering: A strict physical odometer drift window is enforced 
   to block anomalous global cross-pairing.
6. Shortest Angular Distance KNN: Implements circular wrap-around distance logic 
   (e.g., distance between 359° and 1° is evaluated as 2° instead of 358°).
7. Dual File Reporting & Visualizations: Exports master alignment sheets and unmatched 
   anomaly files, auto-saving evaluation charts as high-resolution PNGs.
================================================================================
"""

import os
import tkinter as tk
from tkinter import filedialog, messagebox, ttk
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler

class KNNMatcherGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("Pipeline ILI Data Alignment & KNN Matcher (V2)")
        self.root.geometry("600x500")
        self.root.resizable(False, False)
        
        self.file1_path = ""
        self.file2_path = ""
        self.columns_vars = {}
        
        self.style = ttk.Style()
        self.style.theme_use("clam")
        self.create_widgets()

    def create_widgets(self):
        main_frame = ttk.Frame(self.root, padding="20")
        main_frame.pack(fill=tk.BOTH, expand=True)
        
        title_label = ttk.Label(main_frame, text="ILI Run Alignment & Feature Matcher", 
                                 font=("Helvetica", 16, "bold"), foreground="#2E7D32")
        title_label.pack(pady=(0, 15))
        
        # SECTION 1: File Selection Frame
        file_frame = ttk.LabelFrame(main_frame, text=" 1. Select Inspection Data Files (CSV) ", padding="10")
        file_frame.pack(fill=tk.X, pady=5)
        
        self.lbl_file1 = ttk.Label(file_frame, text="Base Run (e.g., 2005): Not selected", font=("Arial", 9, "italic"))
        self.lbl_file1.grid(row=0, column=0, sticky=tk.W, pady=2)
        btn_file1 = ttk.Button(file_frame, text="Browse Base File", command=self.browse_file1)
        btn_file1.grid(row=0, column=1, sticky=tk.E, padx=5, pady=2)
        
        self.lbl_file2 = ttk.Label(file_frame, text="Target Run (e.g., 2011): Not selected", font=("Arial", 9, "italic"))
        self.lbl_file2.grid(row=1, column=0, sticky=tk.W, pady=2)
        btn_file2 = ttk.Button(file_frame, text="Browse Target File", command=self.browse_file2)
        btn_file2.grid(row=1, column=1, sticky=tk.E, padx=5, pady=2)
        
        file_frame.columnconfigure(0, weight=1)

        # SECTION 2: Feature Checklist Frame
        self.feature_frame = ttk.LabelFrame(main_frame, text=" 2. Select Fields for KNN Distance Matching ", padding="10")
        self.feature_frame.pack(fill=tk.BOTH, expand=True, pady=10)
        
        self.lbl_status = ttk.Label(self.feature_frame, text="Please select both data files to populate available fields.", 
                                    font=("Arial", 10, "italic"), foreground="gray")
        self.lbl_status.pack(pady=20)
        
        self.chk_container = ttk.Frame(self.feature_frame)
        
        # SECTION 3: Action Execution Block
        self.btn_run = ttk.Button(main_frame, text="🚀 Run Spatial Alignment & KNN Matching", 
                                  command=self.execute_processing, state=tk.DISABLED)
        self.btn_run.pack(fill=tk.X, ipady=5, pady=(5, 0))

    def browse_file1(self):
        path = filedialog.askopenfilename(filetypes=[("CSV Files", "*.csv")])
        if path:
            self.file1_path = path
            self.lbl_file1.config(text=f"Base: {os.path.basename(path)}", font=("Arial", 9, "normal"))
            self.populate_features()

    def browse_file2(self):
        path = filedialog.askopenfilename(filetypes=[("CSV Files", "*.csv")])
        if path:
            self.file2_path = path
            self.lbl_file2.config(text=f"Target: {os.path.basename(path)}", font=("Arial", 9, "normal"))
            self.populate_features()

    def populate_features(self):
        if not self.file1_path or not self.file2_path:
            return
        
        try:
            cols1 = pd.read_csv(self.file1_path, nrows=0).columns.tolist()
            cols2 = pd.read_csv(self.file2_path, nrows=0).columns.tolist()
            mutual_cols = sorted(list(set(cols1).intersection(set(cols2))))
            
            self.lbl_status.pack_forget()
            for widget in self.chk_container.winfo_children():
                widget.destroy()
            self.columns_vars.clear()
            
            if not mutual_cols:
                self.lbl_status.pack(pady=20)
                self.lbl_status.config(text="Error: No overlapping column names found between these files.")
                self.btn_run.config(state=tk.DISABLED)
                return
            
            self.chk_container.pack(fill=tk.BOTH, expand=True)
            exclude_defaults = ['depth', 'joint_no', 'item no.', 'wheel']
            
            for index, col in enumerate(mutual_cols):
                var = tk.BooleanVar()
                if col.lower() not in exclude_defaults:
                    var.set(True)
                else:
                    var.set(False)
                    
                self.columns_vars[col] = var
                row = index // 2
                num_col = index % 2
                
                chk = ttk.Checkbutton(self.chk_container, text=col, variable=var)
                chk.grid(row=row, column=num_col, sticky=tk.W, padx=25, pady=4)
                
            self.btn_run.config(state=tk.NORMAL)
            
        except Exception as e:
            messagebox.showerror("Header Parsing Error", f"Could not read column headers:\n{str(e)}")

    def convert_clock_to_degrees(self, val):
        """Converts numerical angles or hh:mm/hh:mm:ss clock positions into degrees."""
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
                # 12 Hours = 360 Degrees -> 1 Hour = 30 Degrees
                total_hours = h + (m / 60.0) + (s / 3600.0)
                return (total_hours * 30.0) % 360.0
            except ValueError:
                return np.nan
        else:
            try:
                return float(val_str) % 360.0
            except ValueError:
                return np.nan

    def execute_processing(self):
        selected_knn_features = [col for col, var in self.columns_vars.items() if var.get()]
        if not selected_knn_features:
            messagebox.showwarning("Selection Missing", "Please select at least one feature field to compute KNN distance metrics.")
            return
        
        try:
            # 1. Read Data
            df1 = pd.read_csv(self.file1_path)
            df2 = pd.read_csv(self.file2_path)
            
            required_keys = ['wheel', 'depth']
            for key in required_keys:
                if key not in df1.columns or key not in df2.columns:
                    raise KeyError(f"Missing required execution key: '{key}' must be present in both CSV files.")
            
            df1_proc, df2_proc = df1.copy(), df2.copy()
            
            # Identify if an angular column exists in selected matching fields
            angular_col = None
            for col in selected_knn_features:
                if col.lower() in ['degrees', 'orientation', 'clock']:
                    angular_col = col
                    break

            # 2. Pre-process Angular Feature if present
            if angular_col:
                print(f"Applying clock-to-degree normalization engine on column: '{angular_col}'")
                df1_proc[angular_col] = df1_proc[angular_col].apply(self.convert_clock_to_degrees)
                df2_proc[angular_col] = df2_proc[angular_col].apply(self.convert_clock_to_degrees)
                
                # Fill missing angles with median
                median_angle = pd.concat([df1_proc[angular_col], df2_proc[angular_col]]).median()
                df1_proc[angular_col] = df1_proc[angular_col].fillna(median_angle if not pd.isna(median_angle) else 0.0)
                df2_proc[angular_col] = df2_proc[angular_col].fillna(median_angle if not pd.isna(median_angle) else 0.0)
                
                # Calculate standard deviation for angular metric scaling
                angular_std = pd.concat([df1_proc[angular_col], df2_proc[angular_col]]).std()
                if angular_std == 0 or np.isnan(angular_std):
                    angular_std = 1.0
                    
            # 3. Process Linear and Categorical Features
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
            
            # Standardize Linear Features using standard machine learning scalar practices
            if encoded_linear_features:
                scaler = StandardScaler()
                scaler.fit(pd.concat([df1_proc[encoded_linear_features], df2_proc[encoded_features := encoded_linear_features]], axis=0))
                X1_linear_scaled = scaler.transform(df1_proc[encoded_linear_features])
                X2_linear_scaled = scaler.transform(df2_proc[encoded_linear_features])
            
            # Clean base required spatial keys 
            df1_proc['wheel'] = df1_proc['wheel'].fillna(0)
            df2_proc['wheel'] = df2_proc['wheel'].fillna(0)
            
            # 4. Spatial Window Filter Loop + Hybrid Circular Distance KNN Matrix
            matched_indices = []
            distances = []
            MAX_ALLOWABLE_DRIFT = 200.0 
            
            print("Running custom wrap-around angular distance matching matrix...")
            for i, row in df1_proc.iterrows():
                w_val = row['wheel']
                
                # Strict physical boundary candidate filter
                candidates = df2_proc[(df2_proc['wheel'] >= w_val - MAX_ALLOWABLE_DRIFT) & 
                                      (df2_proc['wheel'] <= w_val + MAX_ALLOWABLE_DRIFT)]
                
                if len(candidates) == 0:
                    matched_indices.append(None)
                    distances.append(np.nan)
                else:
                    # A. Linear components squared distance
                    if encoded_linear_features:
                        linear_dist_sq = np.sum((X2_linear_scaled[candidates.index] - X1_linear_scaled[i]) ** 2, axis=1)
                    else:
                        linear_dist_sq = np.zeros(len(candidates))
                        
                    # B. Custom Shortest Angular Distance wrap-around logic
                    if angular_col:
                        target_ang = row[angular_col]
                        cand_angs = candidates[angular_col].values
                        
                        # Shortest wrap-around arc distance calculation
                        ang_diff = np.abs(cand_angs - target_ang)
                        shortest_ang_diff = np.minimum(ang_diff, 360.0 - ang_diff)
                        
                        # Scale the periodic distance uniformly by its standard deviation
                        ang_dist_sq = (shortest_ang_diff / angular_std) ** 2
                    else:
                        ang_dist_sq = np.zeros(len(candidates))
                        
                    # Combine metrics into hybrid space Euclidean equivalent
                    total_dists = np.sqrt(linear_dist_sq + ang_dist_sq)
                    best_match_idx = np.argmin(total_dists)
                    
                    matched_indices.append(candidates.index[best_match_idx])
                    distances.append(total_dists[best_match_idx])
                
            # 5. Build Unified Dataframe Dtype-Safe Structure
            matched_rows = []
            nan_row = pd.Series(np.nan, index=df2.columns)
            
            for idx in matched_indices:
                if idx is not None:
                    matched_rows.append(df2.loc[idx])
                else:
                    matched_rows.append(nan_row)
            
            df_matched_2 = pd.DataFrame(matched_rows).reset_index(drop=True)
            df_matched_2.columns = [f"{col}_secondRun" for col in df2.columns]
            
            df_final = pd.concat([df1.reset_index(drop=True), df_matched_2], axis=1)
            df_final['knn_distance'] = distances
            df_final['depth_difference'] = df_final['depth'] - df_final['depth_secondRun']
            df_final['wheel_difference'] = df_final['wheel'] - df_final['wheel_secondRun']
            
            # Export Master File
            out_csv = 'ML2_matched_spatial_depth_change.csv'
            df_final.to_csv(out_csv, index=False)
            
            # Export Unmatched Anomalies File
            valid_mask = [idx is not None for idx in matched_indices]
            df_unmatched = df1[~np.array(valid_mask)].copy()
            unmatched_csv = 'ML2_unmatched_anomalies.csv'
            df_unmatched.to_csv(unmatched_csv, index=False)
            
            # 6. Generate and Automatically Save Individual PNG Plots
            sns.set_theme(style="whitegrid")
            
            fig1, ax1 = plt.subplots(figsize=(7, 5))
            sns.histplot(data=df_final.dropna(subset=['depth_difference']), x='depth_difference', color='#4CAF50', kde=True, bins=40, ax=ax1)
            ax1.axvline(0, color='red', linestyle='--', linewidth=2, label='No Change')
            ax1.set_title('Distribution of Depth Change (Circular Distance Fixed)', fontweight='bold')
            ax1.set_xlabel('Depth Difference (Base - Target)')
            ax1.legend()
            plt.tight_layout()
            fig1.savefig('depth_change_distribution.png', dpi=300)
            
            fig2, ax2 = plt.subplots(figsize=(7, 5))
            sns.scatterplot(x='depth', y='depth_secondRun', data=df_final.dropna(subset=['depth_secondRun']), alpha=0.6, color='#2E7D32', ax=ax2)
            max_val = max(df_final['depth'].dropna().max(), df_final['depth_secondRun'].dropna().max())
            ax2.plot([0, max_val], [0, max_val], color='red', linestyle='--', linewidth=2, label='1:1 Line')
            ax2.set_title('Depth Comparison Scatter Matrix', fontweight='bold')
            ax2.set_xlabel('Depth in Base Run')
            ax2.set_ylabel('Depth in Target Run')
            ax2.legend()
            plt.tight_layout()
            fig2.savefig('depth_comparison_scatter.png', dpi=300)
            
            fig3, ax3 = plt.subplots(figsize=(7, 5))
            sns.histplot(data=df_final.dropna(subset=['wheel_difference']), x='wheel_difference', bins=50, color='#1976D2', kde=True, ax=ax3)
            ax3.axvline(0, color='black', linestyle=':')
            ax3.set_title('Odometer Run Alignment Verification', fontweight='bold')
            ax3.set_xlabel('Odometer Distance Drift Units (Strictly Bounded)')
            plt.tight_layout()
            fig3.savefig('odometer_alignment_drift.png', dpi=300)
            
            total_unmatched = len(df_final) - sum(valid_mask)
            messagebox.showinfo("Process Complete", 
                                f"Success!\n\n"
                                f"1. Master file written to:\n   '{out_csv}'\n\n"
                                f"2. Isolated unmatched baseline anomalies written to:\n   '{unmatched_csv}'\n\n"
                                f"3. Total baseline features processed: {len(df1)}\n"
                                f"4. Pruned anomalies lacking local spatial matches: {total_unmatched}\n\n"
                                f"Click OK to open clean chart views.")
            plt.show()

        except Exception as e:
            messagebox.showerror("Execution Fault", f"An error stopped the data alignment:\n{str(e)}")

if __name__ == "__main__":
    root = tk.Tk()
    app = KNNMatcherGUI(root)
    root.mainloop()