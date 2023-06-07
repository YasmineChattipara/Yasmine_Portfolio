/* Importing Auto_Mpg.data file and analyzing the variables dataset*/
LIBNAME PROJECT "/home/u61480438/BAN110/PROJECT";

Data PROJECT.AUTO_MPG;
	INFILE '/home/u61480438/BAN110/PROJECT/auto-mpg.data';
	INFORMAT Car_Name $30.;
	INPUT @1 Mpg 4.
	  @8 Cylinders 1. 
	  @12 Displacement 5.
	  @23 Horsepower 5.
	  @34 Weight 5.
	  @45 Acceleration 4.
	  @52 Model_Year 2.
	  @56 Origin 1.
	  @58 Car_Name & $30.;
	FORMAT Mpg 4.1 
		   Displacement 5.1 
		   Horsepower 5.1
		   Weight 6.1 
		   Acceleration 4.1;
Run;

TITLE "Analyzing the Auto MPG data";
PROC CONTENTS DATA=PROJECT.AUTO_MPG VARNUM;
RUN;

/* Printing the first ten observations */
TITLE 'First ten observations of the raw Auto_Mpg dataset';

PROC PRINT DATA=PROJECT.AUTO_MPG (OBS=10);
RUN;

/* Descriptive statistics  of Dependent/Target variable MPG	*/
Title 'Descrpitive Statitics for Dependent variable MPG';

Proc Means Data=project.Auto_Mpg;
	Var mpg;
Run;

/* Histogram of Dependent/Target variable MPG */
Title 'Histogram of MPG';

Proc Sgplot Data=project.Auto_Mpg;
	Histogram Mpg;
	Density Mpg;
	Density Mpg / type=kernel;
Run;

/* Working with Categorical Values*/
options nolabel;
Title 'Listing Frequencies for Cylinders Model_year and Origin';

Proc Freq Data=project.Auto_Mpg;
	Tables Cylinders Model_Year Origin / nocum missing;
Run;

/* Checking for missing values in categorical variables by using informat method*/
Proc Format;
	Value Origin_Check 1, 2, 3='Valid' other='Invalid';
	Value Cyl_Check 3, 4, 5, 6, 8='Valid' other='Invalid';
	Value Year_Check 70-82='Valid' other='Invalid';
Run;

Data _null_;
	File Print;
	Set Project.Auto_Mpg (Keep=Car_Name Cylinders Model_Year Origin);

	If put(Cylinders, Cyl_Check.)='Invalid' then
		put 'Missing observation of Cylinders = ' _n_ Car_name Cylinders=;
	Else if Put(Model_Year, Year_Check.)='Invalid' then
		put 'Missing observation of Model_Year = ' _n_ Car_name Model_Year=;
	Else if put(Origin, Origin_Check.)='Invalid' then
		put 'Missing observation of Origin = ' _n_ Car_name Origin=;
Run;

Title 'Checking for Missing values in Categorical variables';

Proc Freq Data=project.Auto_Mpg;
	Tables Cylinders Model_Year Origin / nocum nopercent;
	Format Cylinders Cyl_Check. Model_Year Year_Check. Origin Origin_Check.;
Run;

/* Converting Date from 2 digit number to Date9. format for full year*/
Data project.Auto_Mpg;
	Set project.Auto_Mpg;
	Year_new=Cat('03/01/19', Model_Year);
	Model_Year=year(input(Year_new, mmddyy10.));
	Drop Year_new;
Run;

Title 'Printing first 5 observations after date conversion';

Proc Print Data=project.Auto_Mpg (obs=5);
	Var Car_Name Model_Year;
Run;

/*	Deriving Vehicle brand name and Model from car_Name variable and drop the car_name*/
Data project.Auto_Mpg;
	Set project.Auto_Mpg;
	Car_Name=Propcase(Compress(car_Name, '"'));
	Array model_n [6] $20. Model1-Model6;

	Do i=1 to 6;
		Model_n [i]=compress(Scan(Car_Name, i), "'");
	End;

	If _n_=293 then
		Model3='';
	Brand=Model1;
	Model=Catx('', Model2, model3, model4, model5, model6);
	Drop Model1-Model6 Car_Name i;
Run;
PROC PRINT DATA=project.auto_mpg (obs=5);
RUN;

/*Checking Errors in Brand Variable*/
Title 'Checking errors in Brand variable';

Proc Freq Data=project.Auto_Mpg;
	Tables Brand / nocum nopercent;
Run;

/*Checking and Correcting Spelling errors in Brand variable */
Data project.Auto_Mpg;
	Set project.Auto_Mpg;
	Brand=Tranwrd(Brand, 'Chevy', 'Chevrolet');
	Brand=Tranwrd(Brand, 'Chevroelt', 'Chevrolet');
	Brand=Tranwrd(Brand, 'Hi', 'Honda');
	Brand=Tranwrd(Brand, 'Maxda', 'Mazda');
	Brand=Tranwrd(Brand, 'Vw', 'Volkswagen');
	Brand=Tranwrd(Brand, 'Vokswagen', 'Volkswagen');
	Brand=Tranwrd(Brand, 'Toyouta', 'Toyota');
Run;

Title 'Corrected Brand variable';

Proc Freq Data=project.Auto_Mpg;
	Tables Brand / nocum nopercent;
Run;

/*Working with Numerical Variables*/
options nolabel;

Proc MEans Data=project.Auto_Mpg n nmiss min max mean median mode stddev var;
	Var mpg acceleration displacement weight horsepower;
Run;

/* Checking Missing Numeric Observations */
Title 'Identifying Missing numeric values';

Data _null_;
	File print;
	Set project.Auto_Mpg;
	Array Numeric [*] _NUMERIC_;

	Do i=1 to Dim(Numeric);

		If missing(numeric(i)) then
			put 'Missing Observation ' 
				Brand=Model=Mpg=Cylinders=Displacement=Horsepower=Weight=Acceleration=;
	End;
Run;


/* Treating the missing values of horsepower by impuation of mean value.
	Checking Mean Horsepower for various Cylinder categories */
	
Proc Means Data=project.Auto_Mpg;
	Class Cylinders;
	Var Horsepower;
Run;

/* Replacing missing horsepower with mean horespower grouped by Cylinders */
Proc Sort Data=project.Auto_Mpg;
	by Cylinders;
Run;


Proc Stdize data=project.Auto_Mpg out=project.Auto_Mpg reponly method=mean;
	by cylinders;
Run;

/* A new dervived variable Power-Weight Ratio */
Data project.Auto_Mpg;
	Set project.Auto_Mpg;
	PWR=horsepower/weight;
Run;
TITLE "Listing of Auto MPG(PWR-new derived variable)";
PROC PRINT DATA=project.auto_mpg(obs=5);
RUN;

/* Detecting outliers for numeric variables by using Standarad deviation method(Proc Univariate)
   and checking whether normally distributed*/
Proc Univariate Data=project.Auto_Mpg plots;
	Var mpg acceleration displacement weight horsepower;
Run;

/* After Checking we see variable Acceleration has normal distribution. Hence, we will use
Standard Deviation method to detect Outliers		*/
Proc Means Data=project.Auto_Mpg noprint;
	Var Acceleration;
	Output out=Means (drop=_type_ _freq_) Mean=Std= / autoname;
Run;

Proc Means Data=project.Auto_Mpg noprint;
	Var pwr;
	Output out=IQR (drop=_type_ _freq_) Q1=Q3=Qrange= / autoname;
Run;

/* Detecting Outliers for Acceleration */
Title 'Listing Outliers for Acceleration';

Data _NULL_;
	Set project.Auto_Mpg (keep=Acceleration Brand Model);
	File Print;

	If _n_=1 then
		set Means;

	If Acceleration <=Acceleration_Mean - 2*Acceleration_StdDev or 
		Acceleration > Acceleration_Mean + 2*Acceleration_StdDev then
			Put 'Outlier detected for ' Brand Model ' where Acceleration = ' 
			Acceleration;
Run;

Title 'Listing Outliers for Acceleration';

Data project.Auto_Mpg;
	Set project.Auto_Mpg;

	If _N_=1 then
		set means;

	If Acceleration < Acceleration_Mean - 2*Acceleration_StdDev or 
		Acceleration > Acceleration_Mean + 2*Acceleration_StdDev then
			delete;
	Drop Acceleration_MEan Acceleration_StdDev;
Run;

Proc Univariate Data=project.Auto_Mpg plots;
	Var Acceleration;
Run;

/* Detecting Outliers for Power-Weight Ration using Inter Quartile Range */
Title 'Listing Outliers for Power-Weight Ratio';

Data _NULL_;
	Set project.Auto_Mpg (keep=pwr Brand Model);
	File Print;

	If _n_=1 then
		set IQR;

	If pwr < pwr_Q1 - 1.5*pwr_Qrange or pwr > pwr_Q3 + 1.5*pwr_Qrange then
		Put 'Outlier detected for ' Brand Model ' Power-Weight ratio = ' pwr;
Run;

Title;
Title 'Listing Outliers for Power-Weight Ratio';

Data project.Auto_Mpg;
	Set project.Auto_Mpg;

	If _n_=1 then
		set IQR;

	If pwr < pwr_Q1 - 1.5*pwr_Qrange or pwr > pwr_Q3 + 1.5*pwr then
		delete;
	Drop pwr_Q1 pwr_Q3 pwr_Qrange;
Run;

Title;

/* Checking Skewness of Variable Horsepower using QQplot and Histogram */
Title 'Histogram for Horsepower';

Proc sGplot Data=project.Auto_Mpg;
	Histogram horsepower;
	Density horsepower;
	Density horsepower / type=kernel;
Run;

Proc Gchart Data=project.Auto_Mpg;
	vbar horsepower;
	Run;
	Title 'QQ-Plot for Horsepower';

Proc Univariate Data=project.Auto_Mpg;
	Var horsepower;
	qqplot;
Run;

/* Applying Log10 transformation on Horsepower */
Data Log_test;
	Set project.Auto_Mpg;
	LogHP=Log(horsepower);
Run;

Title 'Histogram of Horsepower after Log Transformation';

Proc sGplot Data=log_test;
	Histogram loghp;
	Density loghp;
	Density loghp/ type=kernel;
Run;

Title 'QQ-Plot of Horsepower after Log Transformation';

Proc Univariate Data=log_test plots;
	Var Loghp;
Run;

Title 'Listing First 5 Observations from Final Dataset';

Proc Print Data=project.Auto_mpg (obs=5);
Run;

Data project.Auto_Mpg;
	Set project.Auto_Mpg;
	Label Brand='Brand of the Vehicle' Model='Model name of vehicle' Cylinders='Number of Cylinders. Categorical Variable which can take following values:
					4, 6 or 8' 
		Model_Year='The year in which the vehicle was manufactured' Origin='Country of Origin of the Vehicle Brand. Has the following categories:
					Unites States  = 1
					Germany =2
					Japan = 3' MPG='City fuel cycle measured in miles/gallon' 
		Displacement='Engine size of vehicle measured in cubic centimetres(CC)' 
		Horsepower='Horsepower of the vehicle' Weight='Weight of vehicle in lbs' 
		Acceleration='Time taken to reach from 0-60 mph' 
		PWR='Power to weight ratio of vehicle measured as hp/lbs';
Run;
*	Test for normality using histogram and QQ plot for Target variable(MPG) Vs 
	Independent variables(horsepower,weight,pwr,displacement) ;
options label;

Proc Contents Data=project.Auto_Mpg;
	ODS Select variables;
Run;

Proc sgplot data=project.Auto_mpg;
	histogram mpg;
	density mpg;
	density mpg / type=kernel;
Run;

Proc sgplot data=project.Auto_mpg;
	reg x=horsepower y=mpg / cli clm;
Run;

Proc sgplot data=project.Auto_mpg;
	reg x=weight y=mpg / cli clm;
Run;

Proc sgplot data=project.Auto_Mpg;
	reg x=pwr y=mpg / cli clm;
Run;

Proc sgplot data=project.Auto_Mpg;
	reg x=displacement y=mpg / cli clm;
Run;

Proc Univariate Data=project.Auto_mpg plots;
	Var mpg;
Run;