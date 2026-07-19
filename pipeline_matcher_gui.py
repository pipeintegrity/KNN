"""
================================================================================
PIPELINE INLINE INSPECTION (ILI) RUN ALIGNMENT & FEATURE MATCHER (THEMED V6)
================================================================================

PROCESS OVERVIEW:
1. Data Loading: User selects two ILI tool run CSV files via the GUI.
2. Dynamic Structural Mapping: Overlapping fields are parsed. Dropdown menus allow 
   the user to dynamically assign which columns represent the Distance/Odometer 
   and Depth features (eliminating hardcoded column dependencies).
3. Auto-Exclusion Checklist & Warnings: The selected Distance and Depth columns are 
   automatically filtered out of the KNN feature block. If a user manually tries 
   to check an active mapping field, an immediate warning triggers to block leakage.
4. Clock-to-Degree Normalization: Automatically converts angular data presented as 
   clock positions ('hh:mm' or 'hh:mm:ss') into standard geometric degrees.
5. Spatial Window Filtering: Enforces a physical distance search window based on the 
   dynamically chosen distance column to block spurious global cross-pairing.
6. Shortest Angular Distance KNN: Evaluates shortest periodic wrap-around arcs.
7. Dual File Reporting & Visualizations: Exports master alignment sheets and unmatched 
   anomaly files, automatically saving high-resolution evaluation charts as PNGs.
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
        self.root.title("Pipeline ILI Data Alignment & KNN Matcher")
        self.root.geometry("600x670")
        self.root.resizable(False, False)
        
        self.file1_path = ""
        self.file2_path = ""
        self.mutual_cols = []
        self.columns_vars = {}
        
        # ---------------------------------------------------------
        # SOFTENED VISUAL THEME CONFIGURATION (TTK STYLE ENGINE)
        # ---------------------------------------------------------
        self.style = ttk.Style()
        self.style.theme_use("clam")
        
        # Muted Corporate Color Palette
        COLOR_BG = "#F0F4F8"          
        COLOR_CARD_BG = "#FFFFFF"     
        COLOR_PRIMARY = "#2B5B84"     
        COLOR_PRIMARY_HOVER = "#3B6F9A"
        COLOR_TEXT = "#334E68"        
        COLOR_BORDER = "#BCCCDC"      
        
        self.root.configure(bg=COLOR_BG)
        self.style.configure(".", background=COLOR_BG, foreground=COLOR_PRIMARY, font=("Helvetica", 10))
        
        self.style.configure("TFrame", background=COLOR_BG)
        self.style.configure("TLabelframe", background=COLOR_CARD_BG, bordercolor=COLOR_BORDER, relief="solid", borderwidth=1)
        self.style.configure("TLabelframe.Label", background=COLOR_CARD_BG, foreground=COLOR_PRIMARY, font=("Helvetica", 10, "bold"))
        
        self.style.configure("TLabel", background=COLOR_BG, foreground=COLOR_PRIMARY)
        self.style.configure("Card.TLabel", background=COLOR_CARD_BG, foreground=COLOR_TEXT)
        self.style.configure("Status.TLabel", background=COLOR_CARD_BG, foreground="#627D98")
        
        self.style.configure("TCheckbutton", background=COLOR_CARD_BG, foreground=COLOR_PRIMARY)
        
        self.style.configure("TButton", 
                             background=COLOR_PRIMARY, 
                             foreground="#FFFFFF", 
                             font=("Helvetica", 10, "bold"),
                             borderwidth=0,
                             focuscolor=COLOR_PRIMARY)
        
        self.style.map("TButton",
                       background=[("active", COLOR_PRIMARY_HOVER), ("disabled", "#D9E2EC")],
                       foreground=[("disabled", "#9FB3C8")])
        
        self.style.configure("TCombobox", 
                             fieldbackground="#FFFFFF", 
                             background=COLOR_PRIMARY, 
                             foreground=COLOR_PRIMARY, 
                             arrowcolor="#FFFFFF",
                             bordercolor=COLOR_BORDER,
                             lightcolor=COLOR_BORDER,
                             darkcolor=COLOR_BORDER)
        self.style.map("TCombobox", 
                       fieldbackground=[("readonly", "#FFFFFF")],
                       foreground=[("readonly", COLOR_PRIMARY)])

        self.create_widgets()

    def create_widgets(self):
        main_frame = ttk.Frame(self.root, padding="20")
        main_frame.pack(fill=tk.BOTH, expand=True)
        
        title_label = ttk.Label(main_frame, text="ILI Run Alignment & Feature Matcher", 
                                 font=("Helvetica", 16, "bold"), foreground=self.style.lookup("TButton", "background"))
        title_label.pack(pady=(0, 15))
        
        # SECTION 1: File Selection Frame
        file_frame = ttk.LabelFrame(main_frame, text=" 1. Select Inspection Data Files (CSV) ", padding="15")
        file_frame.pack(fill=tk.X, pady=5)
        
        self.lbl_file1 = ttk.Label(file_frame, text="Base Run: Not selected", style="Card.TLabel", font=("Arial", 9, "italic"))
        self.lbl_file1.grid(row=0, column=0, sticky=tk.W, pady=4)
        btn_file1 = ttk.Button(file_frame, text="Browse Base File", command=self.browse_file1, width=18)
        btn_file1.grid(row=0, column=1, sticky=tk.E, padx=5, pady=4)
        
        self.lbl_file2 = ttk.Label(file_frame, text="Target Run: Not selected", style="Card.TLabel", font=("Arial", 9, "italic"))
        self.lbl_file2.grid(row=1, column=0, sticky=tk.W, pady=4)
        btn_file2 = ttk.Button(file_frame, text="Browse Target File", command=self.browse_file2, width=18)
        btn_file2.grid(row=1, column=1, sticky=tk.E, padx=5, pady=4)
        file_frame.columnconfigure(0, weight=1)

        # SECTION 2: Dynamic Column Mapping Dropdowns
        mapping_frame = ttk.LabelFrame(main_frame, text=" 2. Map Critical Pipeline Columns ", padding="15")
        mapping_frame.pack(fill=tk.X, pady=5)
        
        lbl_dist = ttk.Label(mapping_frame, text="Distance/Odometer Column:", style="Card.TLabel")
        lbl_dist.grid(row=0, column=0, sticky=tk.W, padx=5, pady=6)
        self.cbo_distance = ttk.Combobox(mapping_frame, state="readonly", width=25)
        self.cbo_distance.grid(row=0, column=1, sticky=tk.E, padx=5, pady=6)
        self.cbo_distance.bind("<<ComboboxSelected>>", self.refresh_checkboxes)
        
        lbl_dpth = ttk.Label(mapping_frame, text="Depth Column:", style="Card.TLabel")
        lbl_dpth.grid(row=1, column=0, sticky=tk.W, padx=5, pady=6)
        self.cbo_depth = ttk.Combobox(mapping_frame, state="readonly", width=25)
        self.cbo_depth.grid(row=1, column=1, sticky=tk.E, padx=5, pady=6)
        self.cbo_depth.bind("<<ComboboxSelected>>", self.refresh_checkboxes)
        mapping_frame.columnconfigure(1, weight=1)

        # SECTION 3: Feature Checklist Frame
        self.feature_frame = ttk.LabelFrame(main_frame, text=" 3. Select Morphological Fields for KNN Distance Matching ", padding="15")
        self.feature_frame.pack(fill=tk.BOTH, expand=True, pady=5)
        
        self.lbl_status = ttk.Label(self.feature_frame, text="Select data files above to populate available features.", 
                                    style="Status.TLabel", font=("Arial", 10, "italic"))
        self.lbl_status.pack(pady=40)
        
        self.chk_container = tk.Frame(self.feature_frame, bg="#FFFFFF")
        
        # SECTION 4: Execution Core Trigger
        self.btn_run = ttk.Button(main_frame, text="🚀 Run Spatial Alignment & KNN Matching", 
                                  command=self.execute_processing, state=tk.DISABLED)
        self.btn_run.pack(fill=tk.X, ipady=6, pady=(10, 0))

    def browse_file1(self):
        path = filedialog.askopenfilename(filetypes=[("CSV Files", "*.csv")])
        if path:
            self.file1_path = path
            self.lbl_file1.config(text=f"Base: {os.path.basename(path)}", font=("Arial", 9, "normal"))
            self.load_mutual_headers()

    def browse_file2(self):
        path = filedialog.askopenfilename(filetypes=[("CSV Files", "*.csv")])
        if path:
            self.file2_path = path
            self.lbl_file2.config(text=f"Target: {os.path.basename(path)}", font=("Arial", 9, "normal"))
            self.load_mutual_headers()

    def load_mutual_headers(self):
        if not self.file1_path or not self.file2_path:
            return
        
        try:
            cols1 = pd.read_csv(self.file1_path, nrows=0).columns.tolist()
            cols2 = pd.read_csv(self.file2_path, nrows=0).columns.tolist()
            self.mutual_cols = sorted(list(set(cols1).intersection(set(cols2))))
            
            if not self.mutual_cols:
                self.lbl_status.pack(pady=40)
                self.lbl_status.config(text="Error: No overlapping column names found between these files.")
                self.btn_run.config(state=tk.DISABLED)
                return
            
            dist_default = ""
            depth_default = ""
            for col in self.mutual_cols:
                if col.lower() in ['wheel', 'chainage', 'distance', 'odometer', 'dist']:
                    dist_default = col
                if col.lower() in ['depth', 'dpth']:
                    depth_default = col
            
            self.cbo_distance['values'] = self.mutual_cols
            if dist_default:
                self.cbo_distance.set(dist_default)
            elif self.mutual_cols:
                self.cbo_distance.current(0)
                
            self.cbo_depth['values'] = self.mutual_cols
            if depth_default:
                self.cbo_depth.set(depth_default)
            elif self.mutual_cols:
                self.cbo_depth.current(min(1, len(self.mutual_cols)-1))
            
            self.lbl_status.pack_forget()
            self.chk_container.pack(fill=tk.BOTH, expand=True)
            self.refresh_checkboxes()
            self.btn_run.config(state=tk.NORMAL)
            
        except Exception as e:
            messagebox.showerror("Header Parsing Error", f"Could not read column headers:\n{str(e)}")

    def refresh_checkboxes(self, event=None):
        if not self.mutual_cols:
            return
        
        current_dist = self.cbo_distance.get()
        current_depth = self.cbo_depth.get()
        
        for widget in self.chk_container.winfo_children():
            widget.destroy()
        self.columns_vars.clear()
        
        exclude_defaults = ['joint_no', 'item no.']
        
        for index, col in enumerate(self.mutual_cols):
            var = tk.BooleanVar()
            if col.lower() not in exclude_defaults and col != current_dist and col != current_depth:
                var.set(True)
            else:
                var.set(False)
                
            self.columns_vars[col] = var
            row = index // 2
            num_col = index % 2
            
            # Integrated lambda trigger command to monitor checkbox interaction states live
            chk = ttk.Checkbutton(self.chk_container, text=col, variable=var,
                                  command=lambda c=col: self.validate_checkbox_selection(c))
            chk.grid(row=row, column=num_col, sticky=tk.W, padx=25, pady=4)

    def validate_checkbox_selection(self, col):
        """Layer 1 Failsafe: Intercepts active checking modifications to block structural leakage."""
        if self.columns_vars[col].get():
            current_dist = self.cbo_distance.get()
            current_depth = self.cbo_depth.get()
            
            if col == current_dist or col == current_depth:
                role_label = "Distance/Odometer" if col == current_dist else "Depth Inference"
                messagebox.showwarning(
                    "Data Leakage Guardrail", 
                    f"Warning: '{col}' is already mapped as the {role_label} variable in Section 2.\n\n"
                    f"Including index attributes or matching targets as morphological features violates machine learning best practices.\n\n"
                    f"Selection has been automatically reverted."
                )
                self.columns_vars[col].set(False)

    def convert_clock_to_degrees(self, val):
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

    def execute_processing(self):
        distance_col = self.cbo_distance.get()
        depth_col = self.cbo_depth.get()
        
        if distance_col == depth_col:
            messagebox.showerror("Mapping Error", "Distance Column and Depth Column cannot be assigned to the same field.")
            return
            
        selected_knn_features = [col for col, var in self.columns_vars.items() if var.get()]
        
        # Layer 2 Failsafe: Hard validation check right before processing arrays
        conflicts = [col for col in selected_knn_features if col == distance_col or col == depth_col]
        if conflicts:
            messagebox.showerror(
                "Execution Blocked", 
                f"Conflict detected: The following features are checked in Section 3 but already mapped in Section 2:\n"
                f"{', '.join(conflicts)}\n\n"
                f"Please uncheck these features before executing the matching matrix."
            )
            return
            
        if not selected_knn_features:
            messagebox.showwarning("Selection Missing", "Please select at least one feature field to compute KNN distance metrics.")
            return
        
        try:
            # 1. Read Data
            df1 = pd.read_csv(self.file1_path)
            df2 = pd.read_csv(self.file2_path)
            
            df1_proc, df2_proc = df1.copy(), df2.copy()
            
            angular_col = None
            for col in selected_knn_features:
                if col.lower() in ['degrees', 'orientation', 'clock']:
                    angular_col = col
                    break

            # 2. Pre-process Angular Feature if present
            if angular_col:
                df1_proc[angular_col] = df1_proc[angular_col].apply(self.convert_clock_to_degrees)
                df2_proc[angular_col] = df2_proc[angular_col].apply(self.convert_clock_to_degrees)
                median_angle = pd.concat([df1_proc[angular_col], df2_proc[angular_col]]).median()
                df1_proc[angular_col] = df1_proc[angular_col].fillna(median_angle if not pd.isna(median_angle) else 0.0)
                df2_proc[angular_col] = df2_proc[angular_col].fillna(median_angle if not pd.isna(median_angle) else 0.0)
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
            
            if encoded_linear_features:
                scaler = StandardScaler()
                scaler.fit(pd.concat([df1_proc[encoded_linear_features], df2_proc[encoded_linear_features]], axis=0))
                X1_linear_scaled = scaler.transform(df1_proc[encoded_linear_features])
                X2_linear_scaled = scaler.transform(df2_proc[encoded_linear_features])
            
            df1_proc[distance_col] = df1_proc[distance_col].fillna(0)
            df2_proc[distance_col] = df2_proc[distance_col].fillna(0)
            
            # 4. Spatial Window Filter Loop (Bounded dynamically by user choice)
            matched_indices = []
            distances = []
            MAX_ALLOWABLE_DRIFT = 200.0 
            
            for i, row in df1_proc.iterrows():
                w_val = row[distance_col]
                candidates = df2_proc[(df2_proc[distance_col] >= w_val - MAX_ALLOWABLE_DRIFT) & 
                                      (df2_proc[distance_col] <= w_val + MAX_ALLOWABLE_DRIFT)]
                
                if len(candidates) == 0:
                    matched_indices.append(None)
                    distances.append(np.nan)
                else:
                    if encoded_linear_features:
                        linear_dist_sq = np.sum((X2_linear_scaled[candidates.index] - X1_linear_scaled[i]) ** 2, axis=1)
                    else:
                        linear_dist_sq = np.zeros(len(candidates))
                        
                    if angular_col:
                        target_ang = row[angular_col]
                        cand_angs = candidates[angular_col].values
                        ang_diff = np.abs(cand_angs - target_ang)
                        shortest_ang_diff = np.minimum(ang_diff, 360.0 - ang_diff)
                        ang_dist_sq = (shortest_ang_diff / angular_std) ** 2
                    else:
                        ang_dist_sq = np.zeros(len(candidates))
                        
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
            
            df_final[f'{depth_col}_difference'] = df_final[depth_col] - df_final[f'{depth_col}_secondRun']
            df_final[f'{distance_col}_difference'] = df_final[distance_col] - df_final[f'{distance_col}_secondRun']
            
            # Export CSV files
            out_csv = 'ML2_matched_spatial_depth_change.csv'
            df_final.to_csv(out_csv, index=False)
            
            valid_mask = [idx is not None for idx in matched_indices]
            df_unmatched = df1[~np.array(valid_mask)].copy()
            unmatched_csv = 'ML2_unmatched_anomalies.csv'
            df_unmatched.to_csv(unmatched_csv, index=False)
            
            # 6. Generate and Automatically Save Plots (Dynamic Axis Labels)
            sns.set_theme(style="whitegrid")
            
            fig1, ax1 = plt.subplots(figsize=(7, 5))
            sns.histplot(data=df_final.dropna(subset=[f'{depth_col}_difference']), x=f'{depth_col}_difference', color='#4CAF50', kde=True, bins=40, ax=ax1)
            ax1.axvline(0, color='red', linestyle='--', linewidth=2, label='No Change')
            ax1.set_title(f'Distribution of Depth Change ({depth_col})', fontweight='bold')
            ax1.set_xlabel(f'Depth Difference ({depth_col} Base - Target)')
            ax1.legend()
            plt.tight_layout()
            fig1.savefig('depth_change_distribution.png', dpi=300)
            
            fig2, ax2 = plt.subplots(figsize=(7, 5))
            sns.scatterplot(x=depth_col, y=f'{depth_col}_secondRun', data=df_final.dropna(subset=[f'{depth_col}_secondRun']), alpha=0.6, color='#2E7D32', ax=ax2)
            max_val = max(df_final[depth_col].dropna().max(), df_final[f'{depth_col}_secondRun'].dropna().max())
            ax2.plot([0, max_val], [0, max_val], color='red', linestyle='--', linewidth=2, label='1:1 Line')
            ax2.set_title(f'Depth Parity: Base vs Target', fontweight='bold')
            ax2.set_xlabel(f'{depth_col} in Base Run')
            ax2.set_ylabel(f'{depth_col} in Target Run')
            ax2.legend()
            plt.tight_layout()
            fig2.savefig('depth_comparison_scatter.png', dpi=300)
            
            fig3, ax3 = plt.subplots(figsize=(7, 5))
            sns.histplot(data=df_final.dropna(subset=[f'{distance_col}_difference']), x=f'{distance_col}_difference', bins=50, color='#1976D2', kde=True, ax=ax3)
            ax3.axvline(0, color='black', linestyle=':')
            ax3.set_title(f'Odometer Drift Validation ({distance_col})', fontweight='bold')
            ax3.set_xlabel(f'{distance_col} Difference (Base - Target)')
            plt.tight_layout()
            fig3.savefig('odometer_alignment_drift.png', dpi=300)
            
            total_unmatched = len(df_final) - sum(valid_mask)
            messagebox.showinfo("Process Complete", 
                                f"Success!\n\n"
                                f"1. Master file saved to: '{out_csv}'\n"
                                f"2. Unmatched list saved to: '{unmatched_csv}'\n\n"
                                f"Mapped Fields:\n"
                                f"- Distance variable: '{distance_col}'\n"
                                f"- Depth variable: '{depth_col}'\n\n"
                                f"Total features processed: {len(df1)}\n"
                                f"Features lacking adjacent spatial matches: {total_unmatched}")
            plt.show()

        except Exception as e:
            messagebox.showerror("Execution Fault", f"An error stopped the data alignment:\n{str(e)}")

if __name__ == "__main__":
    root = tk.Tk()
    app = KNNMatcherGUI(root)
    root.mainloop()