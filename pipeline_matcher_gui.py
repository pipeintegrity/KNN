"""
================================================================================
PIPELINE INLINE INSPECTION (ILI) RUN ALIGNMENT & FEATURE MATCHER
================================================================================

PROCESS OVERVIEW:
1. Data Loading: The user selects two ILI tool run CSV files (Base Run and Target Run).
2. Feature Discovery: The script identifies overlapping, identically named fields 
   between both datasets and populates a GUI checkbox selector.
3. Feature Engineering & Scaling: Non-numeric columns (such as 'INT_EXT') are 
   dynamically mapped to numerical classes across both datasets. Selected features 
   are standardized via Z-score normalization (StandardScaler) to prevent columns 
   with massive absolute ranges (like raw odometer data) from dominating Euclidean 
   distance calculations.
4. Spatial Window Filtering: To ensure physical accuracy and prevent arbitrary 
   morphology jumps across the pipeline, a spatial search constraint is enforced. 
   The target dataset is restricted to a local window surrounding the baseline 
   'wheel' odometer reading before running distance metrics.
5. K-Nearest Neighbor Matching: Within the localized spatial window, a 1-NN model 
   identifies the single closest geometric match using Euclidean distance.
6. Alignment Analysis: The runs are joined side-by-side. The baseline depth is 
   subtracted from the matched target depth to compute change.
7. Reporting: The final combined dataframe is exported to a CSV file, individual 
   diagnostic visualizations are automatically saved as PNGs, and interactive 
   charts are displayed.

CRITICAL ASSUMPTIONS & PREREQUISITES:
- Column Parity: Odometer data ('wheel') and depth data ('depth') must be named 
  identically in lowercase/uppercase across both datasets. 
- Data Value Consistency: Data formats must be standardized before execution. 
  For example, if one file logs internal anomalies as "Int" and the other logs 
  them as "Internal", you must clean/standardize them to match perfectly before 
  running this alignment script.

CRITICAL FIELD EXCLUSION NOTES:
- Odometer & Joint Numbers: Identifiers like 'joint_no' or 'Item No.' must NOT 
  be used as matching features. Joint numbering systems change frequently between 
  runs due to newly added pipeline features, repairs, or odometer slippage.
- Depth Fields: Do NOT select 'depth' as a matching feature under any 
  circumstances. The direct objective of this pipeline analysis is to calculate 
  and infer true depth change (e.g., corrosion growth or sizing tool variance) 
  over time. Using depth to find the nearest neighbor causes total data leakage 
  and invalidates the analysis.
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
        self.root.geometry("600x500")
        self.root.resizable(False, False)
        
        # Data paths and state
        self.file1_path = ""
        self.file2_path = ""
        self.columns_vars = {}
        
        # Set clean GUI style theme
        self.style = ttk.Style()
        self.style.theme_use("clam")
        
        self.create_widgets()

    def create_widgets(self):
        # Main Container
        main_frame = ttk.Frame(self.root, padding="20")
        main_frame.pack(fill=tk.BOTH, expand=True)
        
        # Title Label
        title_label = ttk.Label(main_frame, text="ILI Run Alignment & Feature Matcher", 
                                 font=("Helvetica", 16, "bold"), foreground="#2E7D32")
        title_label.pack(pady=(0, 15))
        
        # ---------------------------------------------------------
        # SECTION 1: File Selection Frame
        # ---------------------------------------------------------
        file_frame = ttk.LabelFrame(main_frame, text=" 1. Select Inspection Data Files (CSV) ", padding="10")
        file_frame.pack(fill=tk.X, pady=5)
        
        # File 1 Selection
        self.lbl_file1 = ttk.Label(file_frame, text="Base Run (e.g., 2005): Not selected", font=("Arial", 9, "italic"))
        self.lbl_file1.grid(row=0, column=0, sticky=tk.W, pady=2)
        btn_file1 = ttk.Button(file_frame, text="Browse Base File", command=self.browse_file1)
        btn_file1.grid(row=0, column=1, sticky=tk.E, padx=5, pady=2)
        
        # File 2 Selection
        self.lbl_file2 = ttk.Label(file_frame, text="Target Run (e.g., 2011): Not selected", font=("Arial", 9, "italic"))
        self.lbl_file2.grid(row=1, column=0, sticky=tk.W, pady=2)
        btn_file2 = ttk.Button(file_frame, text="Browse Target File", command=self.browse_file2)
        btn_file2.grid(row=1, column=1, sticky=tk.E, padx=5, pady=2)
        
        file_frame.columnconfigure(0, weight=1)

        # ---------------------------------------------------------
        # SECTION 2: Feature Checklist Frame
        # ---------------------------------------------------------
        self.feature_frame = ttk.LabelFrame(main_frame, text=" 2. Select Fields for KNN Distance Matching ", padding="10")
        self.feature_frame.pack(fill=tk.BOTH, expand=True, pady=10)
        
        self.lbl_status = ttk.Label(self.feature_frame, text="Please select both data files to populate available fields.", 
                                    font=("Arial", 10, "italic"), foreground="gray")
        self.lbl_status.pack(pady=20)
        
        # Grid container inside feature frame for checkboxes
        self.chk_container = ttk.Frame(self.feature_frame)
        
        # ---------------------------------------------------------
        # SECTION 3: Action Execution Block
        # ---------------------------------------------------------
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
        # Only populate when both files are selected
        if not self.file1_path or not self.file2_path:
            return
        
        try:
            # Efficiently pull only column headers without parsing rows
            cols1 = pd.read_csv(self.file1_path, nrows=0).columns.tolist()
            cols2 = pd.read_csv(self.file2_path, nrows=0).columns.tolist()
            
            # Identify mutual columns across both runs
            mutual_cols = sorted(list(set(cols1).intersection(set(cols2))))
            
            # Clear status placeholder text and old checkboxes
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
            
            # Default smart filters to exclude pipeline identifiers, wheel, and target inference fields
            exclude_defaults = ['depth', 'joint_no', 'item no.', 'wheel']
            
            # Render features in a 2-column clean layout grid
            for index, col in enumerate(mutual_cols):
                var = tk.BooleanVar()
                
                # Check automatically if it's not a spatial key or an identifier
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

    def execute_processing(self):
        # Validate selections
        selected_knn_features = [col for col, var in self.columns_vars.items() if var.get()]
        
        if not selected_knn_features:
            messagebox.showwarning("Selection Missing", "Please select at least one feature field to compute KNN distance metrics.")
            return
        
        print(f"Beginning processing using features: {selected_knn_features}")
        
        try:
            # 1. Read Data
            df1 = pd.read_csv(self.file1_path)
            df2 = pd.read_csv(self.file2_path)
            
            # Verification of required baseline keys
            required_keys = ['wheel', 'depth']
            for key in required_keys:
                if key not in df1.columns or key not in df2.columns:
                    raise KeyError(f"Missing required execution key: '{key}' must be present in both CSV files.")
            
            # 2. Robust Categorical Encoding Pipeline
            df1_proc, df2_proc = df1.copy(), df2.copy()
            encoded_features = []
            
            for col in selected_knn_features:
                # FIXED: Checks if column is non-numeric, capturing object, string, and category dtypes perfectly
                if not pd.api.types.is_numeric_dtype(df1_proc[col]) or not pd.api.types.is_numeric_dtype(df2_proc[col]):
                    # Convert categories dynamically to consistent numeric values across data frames
                    combined_series = pd.concat([df1_proc[col], df2_proc[col]]).astype('category')
                    mapping = dict(enumerate(combined_series.cat.categories))
                    reverse_mapping = {v: k for k, v in mapping.items()}
                    
                    enc_col_name = f"{col}_encoded"
                    df1_proc[enc_col_name] = df1_proc[col].map(reverse_mapping)
                    df2_proc[enc_col_name] = df2_proc[col].map(reverse_mapping)
                    encoded_features.append(enc_col_name)
                else:
                    encoded_features.append(col)
            
            # 3. Apply Standard Scale Normalization
            scaler = StandardScaler()
            scaler.fit(pd.concat([df1_proc[encoded_features], df2_proc[encoded_features]], axis=0))
            
            X1_scaled = pd.DataFrame(scaler.transform(df1_proc[encoded_features]), columns=encoded_features)
            X2_scaled = pd.DataFrame(scaler.transform(df2_proc[encoded_features]), columns=encoded_features)
            
            X1_scaled['wheel'] = df1_proc['wheel']
            X2_scaled['wheel'] = df2_proc['wheel']
            X2_scaled['orig_idx'] = df2_proc.index
            
            # 4. Spatial Window Filter Loop + Focused Core KNN Distance Selection
            matched_indices = []
            distances = []
            window_size = 50.0
            
            for i, row in X1_scaled.iterrows():
                w_val = row['wheel']
                
                # Check spatial constraint block
                candidates = X2_scaled[(X2_scaled['wheel'] >= w_val - window_size) & 
                                       (X2_scaled['wheel'] <= w_val + window_size)]
                
                # Expand dynamically if needed
                if len(candidates) == 0:
                    candidates = X2_scaled[(X2_scaled['wheel'] >= w_val - 500) & 
                                           (X2_scaled['wheel'] <= w_val + 500)]
                if len(candidates) == 0:
                    candidates = X2_scaled
                
                c_feats = candidates[encoded_features].values
                t_feat = row[encoded_features].values.reshape(1, -1)
                
                dists = np.linalg.norm(c_feats - t_feat, axis=1)
                best_match_idx = np.argmin(dists)
                
                matched_indices.append(candidates.iloc[best_match_idx]['orig_idx'])
                distances.append(dists[best_match_idx])
                
            # 5. Build Unified Dataframe Joined Structure
            df_matched_2 = df2.iloc[[int(x) for x in matched_indices]].reset_index(drop=True)
            df_matched_2.columns = [f"{col}_secondRun" for col in df_matched_2.columns]
            
            df_final = pd.concat([df1.reset_index(drop=True), df_matched_2], axis=1)
            df_final['knn_distance'] = distances
            df_final['depth_difference'] = df_final['depth'] - df_final['depth_secondRun']
            
            # Export output CSV
            out_csv = 'ML2_matched_spatial_depth_change.csv'
            df_final.to_csv(out_csv, index=False)
            
            # 6. Generate and Automatically Save Individual PNG Plots
            sns.set_theme(style="whitegrid")
            
            # Plot A: Hist of Depth Change
            fig1, ax1 = plt.subplots(figsize=(7, 5))
            sns.histplot(df_final['depth_difference'], color='#4CAF50', kde=True, bins=40, ax=ax1)
            ax1.axvline(0, color='red', linestyle='--', linewidth=2, label='No Change')
            ax1.set_title('Distribution of Depth Change', fontweight='bold')
            ax1.set_xlabel('Depth Difference (Base - Target)')
            ax1.legend()
            plt.tight_layout()
            fig1.savefig('depth_change_distribution.png', dpi=300)
            
            # Plot B: Scatter Parity
            fig2, ax2 = plt.subplots(figsize=(7, 5))
            sns.scatterplot(x='depth', y='depth_secondRun', data=df_final, alpha=0.6, color='#2E7D32', ax=ax2)
            max_val = max(df_final['depth'].max(), df_final['depth_secondRun'].max())
            ax2.plot([0, max_val], [0, max_val], color='red', linestyle='--', linewidth=2, label='1:1 Line')
            ax2.set_title('Depth Comparison Scatter Matrix', fontweight='bold')
            ax2.set_xlabel('Depth in Base Run')
            ax2.set_ylabel('Depth in Target Run')
            ax2.legend()
            plt.tight_layout()
            fig2.savefig('depth_comparison_scatter.png', dpi=300)
            
            # Plot C: Wheel Drift Quality
            fig3, ax3 = plt.subplots(figsize=(7, 5))
            w_drift = df_final['wheel'] - df_final['wheel_secondRun']
            sns.histplot(w_drift, bins=50, color='#1976D2', kde=True, ax=ax3)
            ax3.set_xlim(-200, 200)
            ax3.axvline(0, color='black', linestyle=':')
            ax3.set_title('Odometer Run Alignment Verification', fontweight='bold')
            ax3.set_xlabel('Odometer Distance Drift Units')
            plt.tight_layout()
            fig3.savefig('odometer_alignment_drift.png', dpi=300)
            
            # Notify User of successful data storage
            messagebox.showinfo("Process Complete", 
                                f"Success!\n\n1. Output file written to: '{out_csv}'"
                                f"\n2. Three individual evaluation plots saved successfully (.png files)."
                                f"\n\nClick OK to open interactive chart panels.")
            
            # Open interactive view windows
            plt.show()

        except Exception as e:
            messagebox.showerror("Execution Fault", f"An error stopped the data alignment:\n{str(e)}")

if __name__ == "__main__":
    root = tk.Tk()
    app = KNNMatcherGUI(root)
    root.mainloop()