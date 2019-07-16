## DATA DICTIONARY
The tidy data folder contains the processed data files needed to recreate the tables, figures, and statistical analyses in the _Scientific Reports_ article: "Cold Exposure Induces Dynamic, Heterogeneous Alterations in Human Brown Adipose Tissue Lipid Content." 

### Tidy Data Files  
The following comma-separated-value (.csv) files are imported and analyzed in the R markdown code:

1. data\_tab2\_subject\_demographics.csv
2. data\_fig3\_cooldose\_simulation.csv
3. data\_fig4-7\_mri\_results.csv
4. data\_figS1\_temp\_temp\_simulation.csv  

### Variable Naming Conventions  
Variable naming conventions and units for each data file are as follows: 

#### data\_tab2\_subject\_demographics  
* Columns: 24
* Rows: 8
* Variables:
    * ID - Subject Identification # (non-sequential)  
    * Sex - Subject Sex - "M" or "F" (male / female)  
    * Age - Subject Age - (years)  
    * Ht_cm - Subject Height - (centimeters)  
    * Mass_kg - Subject Mass - (kilograms)  
    * BMI - Subject Body Mass Index (BMI) - (kilograms/meters^2)  
    * WC - Subject Waist Circumference (WC) - (centimeters)  
    * BSA - Subject Body Surface Area (BSA) - (meters^2)  
    * PCP_Out_t - outdoor temperature at the start of the perception-based cooling protocol (PCP) session - (°C)  
    * PCP_Out_h - outdoor humidity at the start of the perception-based cooling protocol (PCP) session - (%)  
    * PCP_In_t - indoor temperature at the start of the perception-based cooling protocol (PCP) session - (°C)  
    * PCP_In_h - indoor humidity at the start of the perception-based cooling protocol (PCP) session - (%)  
    * Out_t - outdoor temperature at the start of the MRI cooling protocol session - (°C)  
    * Out_h - outdoor humidity at the start of the MRI cooling protocol session - (%)  
    * In_t - indoor temperature at the start of the MRI cooling protocol session - (°C)  
    * In_h - indoor humidity at the start of the MRI cooling protocol session - (%)  
    * TN_t - set water temperature for the Blanketrol cooling unit at thermoneutrality (TN) measured during the PCP session - (°C) 
    * Shiv_t - set water temperature for the Blanketrol cooling unit at sustained shivering measured during the PCP session - (°C)  
    * High_t - shiver threshold water temperature + 6 °C for the MRI cooling protocol session - (°C)  
    * Low_t - shiver threshold water temperature + 3 °C for the MRI cooling protocol session - (°C)  
    * PCPCoolDose - total cooling dose measured during the PCP session - (°C\*min)  
    * PCPCDNorm - total normalized cooling dose measured during the PCP session; normalized with the subject's BSA - (°C\*min\*m^2)  
    * MRICoolDose - total cooling dose measured during the MRI cooling protocol session - (°C\*min)  
    * MRICDNorm - total normalized cooling dose measured during the MRI cooling protocol session; normalized with the subject's BSA - (°C\*min\*m^2)  

#### data\_fig3\_cooldose\_simulation.csv  
* Columns: 9  
* Rows: 62  
* Variables:  
    * Duration - simulated cooling time data - (min)  
    * Temp 1 - simulated water temperature data for a linear cooling gradient - (°C)  
    * Temp 2 - simulated water temperature data for a step cooling gradient - (°C)  
    * Temp1Rel - simulated relative water temperature (Thermoneutral Temperature - Temp1) data for a linear cooling gradient - (°C)  
    * Temp2Rel - simulated relative water temperature (Thermoneutral Temperature - Temp2) data for a step cooling gradient - (°C)  
    * CD1 - simulated cooling dose data (area under the Temp1Rel vs. Duration curve) for a linear cooling gradient - (°C\*min)  
    * CD2 - simulated cooling dose data (area under the Temp2Rel vs. Duration curve) for a step cooling gradient - (°C\*min)  
    * CD1Norm - simulated normalized cooling dose data (cooling dose / body surface area) for a linear cooling gradient applied to a subject with body surface area of 1.6 m^2 - (°C\*min\*m^2)  
    * CD2Norm - simulated normalized cooling dose data (cooling dose / body surface area) for a step cooling gradient applied to a subject with body surface area of 2.1 m^2 - (°C\*min\*m^2)  

#### data\_fig4-7\_mri\_results.csv
* Columns: 18  
* Rows: 7290  
* Variables:  
    * ID - Subject Identification # (non-sequential)  
    * BSA - Subject Body Surface Area (BSA) - (meters^2)   
    * nScan - MRI scan number - (#)  
    * time - time at the start of the MRI acquisition - (min)  
    * Set - set water temperature for the Blanketrol cooling unit at the start of the MRI acquisition - (°C)  
    * Water - actual water temperature for the Blanketrol cooling unit at the start of the MRI acquisition - (°C)  
    * Relative - relative water temperature (Thermoneutral Temperature - Actual Water Temperature) at the start of the MRI acquisition - (°C)  
    * CoolDose - cumulative cooling dose (area under the relative water temperature vs. time curve) at the start of the MRI acquisition - (°C\*min)  
    * CDNorm - cumulative normalized cooling dose (cooling dose / body surface area) at the start of the MRI acquisition - (°C\*min\*m^2)  
    * tGUI - thermal sensation value measured via a thermoesthesia graphical user interface - (arbitrary units) - (50 = Thermoneutral, 0 = Very Cold)  
    * ROIName - region of interest (ROI) label  
        * BATR = right brown adipose tissue (BAT)  
        * BATL = left BAT  
        * WATR = right white adipose tissue (WAT)  
        * WATL = left WAT  
        * MUSR = right skeletal muscle (MUS)  
        * MUSL = left MUS  
        * BAT = right + left BAT  
        * WAT = right + left WAT  
        * MUS = right + left MUS  
        * BAT40t = right + left BAT with fat-signal fraction (FSF) values between 40-100%  
        * BAT50t = right + left BAT with fat-signal fraction values between 50-100%  
        * BAT0 = BAT 0% decile (voxels with FSF values >0 and <10% at thermoneutral)  
        * BAT10 = BAT 10% decile (voxels with FSF values >=10 and <20% at thermoneutral)  
        * BAT20 = BAT 20% decile (voxels with FSF values >=20 and <30% at thermoneutral)  
        * BAT30 = BAT 30% decile (voxels with FSF values >=30 and <40% at thermoneutral)  
        * BAT40 = BAT 40% decile (voxels with FSF values >=40 and <50% at thermoneutral)  
        * BAT50 = BAT 50% decile (voxels with FSF values >=50 and <60% at thermoneutral)  
        * BAT60 = BAT 60% decile (voxels with FSF values >=60 and <70% at thermoneutral)  
        * BAT70 = BAT 70% decile (voxels with FSF values >=70 and <80% at thermoneutral)  
        * BAT80 = BAT 80% decile (voxels with FSF values >=80 and <90% at thermoneutral)  
        * BAT90 = BAT 90% decile (voxels with FSF values >=90 and <=100% at thermoneutral)  
        * WAT0 = WAT 0% decile (voxels with FSF values >0 and <10% at thermoneutral)  
        * WAT10 = WAT 10% decile (voxels with FSF values >=10 and <20% at thermoneutral)  
        * WAT20 = WAT 20% decile (voxels with FSF values >=20 and <30% at thermoneutral)  
        * WAT30 = WAT 30% decile (voxels with FSF values >=30 and <40% at thermoneutral)  
        * WAT40 = WAT 40% decile (voxels with FSF values >=40 and <50% at thermoneutral)  
        * WAT50 = WAT 50% decile (voxels with FSF values >=50 and <60% at thermoneutral)  
        * WAT60 = WAT 60% decile (voxels with FSF values >=60 and <70% at thermoneutral)  
        * WAT70 = WAT 70% decile (voxels with FSF values >=70 and <80% at thermoneutral)  
        * WAT80 = WAT 80% decile (voxels with FSF values >=80 and <90% at thermoneutral)  
        * WAT90 = WAT 90% decile (voxels with FSF values >=90 and <=100% at thermoneutral)  
        * MUS0 = MUS 0% decile (voxels with FSF values >0 and <10% at thermoneutral)  
        * MUS10 = MUS 10% decile (voxels with FSF values >=10 and <20% at thermoneutral)  
        * MUS20 = MUS 20% decile (voxels with FSF values >=20 and <30% at thermoneutral)  
        * MUS30 = MUS 30% decile (voxels with FSF values >=30 and <40% at thermoneutral)  
        * MUS40 = MUS 40% decile (voxels with FSF values >=40 and <50% at thermoneutral)  
        * MUS50 = MUS 50% decile (voxels with FSF values >=50 and <60% at thermoneutral)  
        * MUS60 = MUS 60% decile (voxels with FSF values >=60 and <70% at thermoneutral)  
        * MUS70 = MUS 70% decile (voxels with FSF values >=70 and <80% at thermoneutral)  
        * MUS80 = MUS 80% decile (voxels with FSF values >=80 and <90% at thermoneutral)  
        * MUS90 = MUS 90% decile (voxels with FSF values >=90 and <=100% at thermoneutral)  
    * ROISize - number of voxels included in the ROI - #  
    * Mean - mean fat-signal fraction (FSF) of all voxels in the ROI - (%)  
    * Median - median fat-signal fraction (FSF) of all voxels in the ROI - (%)  
    * SD - standard deviation fat-signal fraction (FSF) of all voxels in the ROI - (%)  
    * SE - standard error fat-signal fraction (FSF) of all voxels in the ROI - (%)  
    * CI_L - lower limit of the 95% confidence interval (bootstrapped w/ 1000 samples) of the mean fat-signal fraction (FSF) of all voxels in the ROI - (%)  
    * CI_H - upper limit of the 95% confidence interval (bootstrapped w/ 1000 samples) of the mean fat-signal fraction (FSF) of all voxels in the ROI - (%)  

#### data\_S1\_temp\_simulation.csv
* Columns: 11  
* Rows: 77 
* Variables:  
    * Temperature - simulated change in brown adipose tissue temperature - (°C)    
    * FSF_0.05 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.05 or 5% (i.e. FSF Decade 0%) - (FSF Fraction (0 to 1))      
    * FSF_0.15 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.15 or 15% (i.e. FSF Decade 10%) - (FSF Fraction (0 to 1))   
    * FSF_0.25 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.25 or 25% (i.e. FSF Decade 20%) - (FSF Fraction (0 to 1))   
    * FSF_0.35 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.35 or 35% (i.e. FSF Decade 30%) - (FSF Fraction (0 to 1))   
    * FSF_0.45 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.45 or 45% (i.e. FSF Decade 40%) - (FSF Fraction (0 to 1))   
    * FSF_0.55 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.55 or 55% (i.e. FSF Decade 50%) - (FSF Fraction (0 to 1))   
    * FSF_0.65 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.65 or 65% (i.e. FSF Decade 60%) - (FSF Fraction (0 to 1))   
    * FSF_0.75 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.75 or 75% (i.e. FSF Decade 70%) - (FSF Fraction (0 to 1))   
    * FSF_0.85 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.85 or 85% (i.e. FSF Decade 80%) - (FSF Fraction (0 to 1))   
    * FSF_0.95 - estimated bias in brown adipose tissue voxels containing a mean fat-signal fraction 0.95 or 95% (i.e. FSF Decade 90%) - (FSF Fraction (0 to 1))   
    
    
