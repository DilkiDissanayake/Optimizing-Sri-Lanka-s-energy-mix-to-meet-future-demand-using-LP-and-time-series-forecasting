### Linear Programming
1
2 %% Clear Everything
3 clc , clear , close all
4
5 %% Defining Parameters
6
7 % Sources
8 months = 12;
9 sources = {’Hydro ’, ’Oil ’, ’Wind ’, ’Coal ’, ’Private ’};
10 n_ sources = length ( sources ) ;
11
12 % Avg . Costs ( LKR/GWh )
13 costs = [2170000 , 56860000 , 24750000 , 26810000 , 69230000];
14
15 % Max capacity ( GWh ) for each source for each month
16 max_ capacity = [
17 1017.63 , 1017.63 , 1017.63 , 1017.63 , 1017.63 , 1017.63 , 1017.63 ,
1017.63 , 1017.63 , 1017.63 , 1017.63 , 1017.63; % Hydro
18 576.72 , 576.72 , 576.72 , 576.72 , 576.72 , 576.72 , 576.72 , 576.72 ,
576.72 , 576.72 , 576.72 , 576.72 % Oil
19 74.52 , 74.52 , 74.52 , 74.52 , 74.52 , 74.52 , 74.52 , 74.52 , 74.52 ,
74.52 , 74.52 , 74.52; % Wind
20 648 , 648 , 648 , 648 , 648 , 648 , 648 , 648 , 648 , 648 , 648 , 648;
% Coal
21 837.44 , 837.44 , 837.44 , 837.44 , 837.44 , 837.44 , 837.44 , 837.44 ,
837.44 , 837.44 , 837.44 , 837.44; % Private
22 ];
23
24 % Monthly energy demand ( GWh )
25 demand = [1174.89 , 1131.76 , 1186.59 , 1100.66 , 1249.45 , 1218.90 ,
1216.43 , 1247.89 , 1197.23 , 1196.49 , 1175.89 , 1171.19];
26
27 % Grid losses (10% loss included in demand )
28 % Adjust for 10% losses
29 demand = demand * 1.1;
30
31 % Minimum generation ( Coal and Oil )
32 min_ coal = 432; % GWh
33 min_oil = 59.04; % GWh
34
35 % Monsoon months and corresponding coal minimum generation
36 monsoon _ months = [5 , 6 , 7 , 10 , 11 , 12]; % May , June , July , October ,
November , December
37 non_ monsoon _ months = setdiff (1:12 , monsoon _ months ) ;
38
39 %% Defining Variables
40
41 % Decision variables : Each source ’s generation for 12 months
42 % x = [x_hydro , x_oil , x_wind , x_coal , x_ private ]
43 x_ size = n_ sources * months ;
44
45 % Upper and lower bounds
46 lb = zeros (x_size , 1);
47 ub = reshape (max _ capacity ’, [] , 1) ;
48
49 % Minimum coal generation bounds
50 coal _idx = 3 * months + (1: months ) ; % Indices for Coal
51 lb ( coal _ idx ( non _ monsoon _ months ) ) = 432; % Minimum coal generation in
non - monsoon months
52 lb ( coal _ idx ( monsoon _ months ) ) = 216; % Minimum coal generation in
monsoon months
53
54 % Minimum oil generation bounds
55 oil_idx = months + (1: months ) ; % Indices for Oil
56 lb ( oil_ idx ) = min _oil ;
57
58 %% Objective Function
59
60 % Objective function : Minimize cost
61 c = repmat ( costs , 1 , months ) ;
62
63 %% Constraints
64
65 % Equality constraints ( demand satisfaction )
66 Aeq = zeros ( months , x_ size ) ;
67 for t = 1: months
68 Aeq (t, t: months :end) = 1;
69 end
70 beq = demand ’;
71
72 % Enforce minimum generation for base load sources ( Coal and Oil)
73 coal _idx = 3 * months + (1: months ); % Indices for Coal
74 oil_idx = months + (1: months ); % Indices for Oil
75 lb( coal _ idx ) = min _ coal ;
76 lb( oil_ idx ) = min _oil ;
77
78 % Hydro Priority in Monsoon Months
79 A_ hydro _ priority = zeros ( length ( monsoon _ months ), x_ size );
80
81 % Loop over monsoon months and create constraints for Hydro priority
82 for t = 1: length ( monsoon _ months )
83 month _idx = monsoon _ months (t);
84
85 % Hydro generation should be maximized first for monsoon months
86 A_ hydro _ priority (t, month _idx) = -1; % Hydro (
negative because we want it to be >= 0)
87 A_ hydro _ priority (t, month _idx + months ) = 1; % Oil
88 A_ hydro _ priority (t, month _idx + 2* months ) = 1; % Wind
89 A_ hydro _ priority (t, month _idx + 3* months ) = 1; % Coal
90 A_ hydro _ priority (t, month _idx + 4* months ) = 1; % Private
91 end
92
93 % The right - hand side for this constraint will be 0 ( Hydro should be
maximized before others )
94 b_ hydro _ priority = zeros ( length ( monsoon _ months ), 1);
95
96 % Combine the new priority constraint into the existing ones
97 A = [A_ hydro _ priority ; Aeq ];
98 b = [b_ hydro _ priority ; beq ];
99
100 % Hydro Generation Limit in Non - Monsoon Months (40% of Max Capacity )
101
102 A_ hydro _ limit = zeros ( length (non_ monsoon _ months ), x_ size );
103
104 % Set Hydro limit to 40% of its max capacity for non - monsoon months
105 for t = 1: length (non_ monsoon _ months )
106 month _idx = non_ monsoon _ months (t);
107 A_ hydro _ limit (t, month _idx) = 1; % Hydro
generation
108 end
109
110 b_ hydro _ limit = repmat (0.4 * max_ capacity (1 , 1) , length (non_ monsoon _
months ), 1); %40% of max capacity
111
112 % Add these new constraints to the existing constraints
113 A = [A; A_ hydro _ limit ];
114 b = [b; b_ hydro _ limit ];
115
116 %% Solving the Problem
117
118 options = optimoptions (’linprog ’, ’Algorithm ’, ’dual - simplex ’, ’
Display ’, ’iter ’);
119 [x, fval , exitflag , output ] = linprog (c, [] , [] , A, b, lb , ub ,
options );
120
121 % Reshape the Solution for Interpretation
122 if isempty (x)
123 error (’Optimization failed : No feasible solution found .’);
124 else
125 x_ reshaped = reshape (x, months , n_ sources )’; % Reshape the
solution
126 end
127
128 %% Display Results
129
130 disp (’Optimal energy generation (GWh by source and month ):’) ;
131 disp ( array2table ( x_ reshaped , ’RowNames ’, sources , ’ VariableNames ’,
..733
132 arrayfun ( @ ( x ) sprintf (’Month _%d’, x ) , 1: months , ’ UniformOutput ’,
false ) ) ) ;
133
134 % Calculate the monthly costs (sum of generation for each source
times its cost )
135 monthly _ costs = sum ( x_ reshaped .* costs ’, 1); % Multiply each month ’s
generation with its corresponding source cost
136
137 disp (’Optimal cost for each month :’) ;
138 disp ( array2table ( monthly _costs , ’RowNames ’, {’Total _ Cost ’} , ’
VariableNames ’, ...
139 arrayfun ( @ ( x ) sprintf (’Month _%d’, x ) , 1: months , ’ UniformOutput ’,
false ) ) ) ;
140
141 disp ([ ’Total cost : ’, num2str ( fval ) , ’ LKR ’]) ;
142
143 % Plotting the Energy Mix
144 figure ;
145 bar (1: months , x_ reshaped ’, ’stacked ’);
146 xlabel (’Months ’);
147 ylabel (’Energy Generation ( GWh )’);
148 title (’Energy Generation by Source ’);
149 legend ( sources , ’Location ’, ’ northoutside ’, ’Orientation ’, ’
horizontal ’);
150 colormap ( lines (n_ sources ));
151 grid on;

###Time series and forecasting

1 # Load libraries
2 library ( tidyverse )
3 library ( lubridate )
4 library ( forecast )
5 library ( tseries )
6 library ( xgboost )
7 library ( seastests )
8 library ( seasonal )
9 library ( keras )
10 library ( TSA )
11 library ( fpp2 )
12 library ( fable )
13 library ( dplyr )
14 library ( ggplot2 )
15 library ( tensorFlow )
16
17 # Load data
18 data = Energy _ Consumption _ 2015 _to_ 2023 _NEW
19
20
21 # Data Preparation
22 data $ Date <- as. Date ( paste ( data $YEAR , data $MONTH , "1", sep = "-") ,
format = "%Y -%m -%d")
23 missing _ data <- sum (is.na( data $ TOTAL . WITH . LECO ) )
24 if ( missing _ data == 0) print ("No Missing Values ") else print (" There
is Missing Values ")
25 par( mfrow = c(1 ,2) )
26 data _ts <- ts( data $ TOTAL . WITH . LECO , start =c(2015 , 1) , frequency =12)
27 data _ Generation _ with _ outliers <- as. numeric ( data _ts)
28 Q1 <- quantile ( data _ Generation _ with _ outliers , 0.25)
29 Q3 <- quantile ( data _ Generation _ with _ outliers , 0.75)
30 IQR <- Q3 - Q1
31 lower _ limit <- Q1 - 1.5 * IQR
32 upper _ limit <- Q3 + 1.5 * IQR
33 cat(" Lower Limit :", lower _limit , "\n")
34 cat(" Upper Limit :", upper _limit , "\n")
35 outliers <- data _ Generation _ with _ outliers [ data _ Generation _ with _
outliers < lower _ limit | data _ Generation _ with _ outliers > upper _
limit ]
36 cat(" Outliers :", outliers , "\n")
37 data _ Generation _ with _ outliers [ data _ Generation _ with _ outliers < lower _
limit ] <- lower _ limit
38 data _ Generation _ with _ outliers [ data _ Generation _ with _ outliers > upper _
limit ] <- upper _ limit
39 boxplot ( data _ Generation _ with _ outliers , main =" Boxplot with Outliers ",
ylab =" Generation ", col=" skyblue ", horizontal = FALSE )
40 abline ( h =c( lower _limit , upper _ limit ) , col ="red ", lty =2)
41
42 # Plot the data
43 plot ( data _ts , main =" Energy Generation (2015 -2023) ", ylab =" Energy (GWh
)", xlab =" Year ")
44 data _ts_ after _ remove _ outliers <- ts( data _ Generation _ with _ outliers ,
start =c(2015 , 1) , frequency =12)
45 plot ( data _ts_ after _ remove _ outliers , main =" Energy Generation
(2015 -2023) ", ylab =" Energy (GWh )", xlab =" Year ")
46 data _ Generation <- as. numeric ( data _ts_ after _ remove _ outliers )
47
48 # ARIMA / SARIMA Model
49 #
-- --- --- --- --- --- -- --- --- --- --- --- --- --- --- --- --- --- --- --- -- --- --- --- --- --- --- -50 # Check stationarity using ADF test
51 adf_ test _ results <- adf . test ( data _ Generation )
52 if ( adf_ test _ results $p . value <= 0.05) print (" Data is stationary ")
else print (" Data is not stationary ")
53
54 # Regular differencing (to remove trend )
55 d <- ndiffs ( data _ Generation )
56 cat(" Number of regular differences needed : ", d , "\n")
57
58 # Seasonal differencing (to remove seasonality )
59 D <- nsdiffs ( data _ts_ after _ remove _ outliers )
60 cat(" Number of seasonal differences needed : ", D, "\n")
61
62 # Differencing the series
63 diff _ data _ Generation <- diff ( data _ Generation )
64 plot .ts( diff _ data _ Generation )
65
66 # Check Seasonality
67 if ( isSeasonal ( diff _ data _ Generation , freq = 12) ) print (" Data have
Seasonality ") else print (" Data do not have Seasonality ")
68 # nsdiffs (ts( diff _ data _ Generation , frequency = 12))
70 # Plot ACF and PACF
71 acf ( diff _ data _ Generation , main ="ACF", lag .max = 36)
72 pacf ( diff _ data _ Generation , main =" PACF ")
73
74 # Differencing the series
75 diff2 _ data _ Generation <- diff ( diff _ data _ Generation , lag = 12)
76 plot .ts( diff2 _ data _ Generation )
77
78 # Check Seasonality
79 if ( isSeasonal ( diff2 _ data _ Generation , freq = 12) ) print (" Data have
Seasonality ") else print (" Data do not have Seasonality ")
80
81 # Plot ACF and PACF
82 acf ( diff2 _ data _ Generation , main ="ACF", lag .max = 36)
83 pacf ( diff2 _ data _ Generation , main =" PACF ")
84 par( mfrow = c(1 ,2) )
85 acf ( diff _ data _ Generation , main ="ACF", lag .max = 36)
86 pacf ( diff2 _ data _ Generation , main ="ACF ", lag .max = 36)
87 dev.off ()
88
89 # SARIMA Implementation
90 # Auto - select SARIMA parameters
91 auto _ sarima _ model <- auto . arima ( data _ Generation , seasonal = TRUE )
92 summary ( auto _ sarima _ model )
93
94 # Manually - select SARIMA parameters
95 sarima _ model <- Arima ( data _ Generation , order = c(2 ,1 ,1) , seasonal =
list ( order =c(2 ,1 ,2) , period =12) )
96 summary ( sarima _ model )
97
98 # Define maximum values for p, q, P, Q
99 max_p <- 3
100 max_P <- 3
101 max_q <- 3
102 max_Q <- 3
103
104 # Placeholder to store the results
105 results <- data . frame ( p = integer () , d = integer () , q = integer () ,
106 P = integer () , D = integer () , Q = integer () ,
107 AIC = numeric () )
108
109 # Loop through all combinations of p, q, P, Q
110 for ( p in 0: max_p ) {
111 for (q in 0: max_q) {
112 for ( P in 0: max_P ) {
113 for (Q in 0: max _Q) {
114 # Try fitting the ARIMA model
115 tryCatch ({
116 model <- Arima ( data _ Generation , order = c(p , 1 , q) ,
117 seasonal = list ( order = c(P , 1 , Q) , period =
12) )
118 # Store the results
119 results <- rbind ( results , data . frame ( p = p , d = 1 , q = q,
120 P = P , D = 1 , Q = Q,
121 AIC = AIC ( model ) ) )
122 } , error = function ( e ) {
123 # Ignore models that fail
124 NULL
125 })
126 }
127 }
128 }
129 }
130
131 # Sort the results by AIC to find the best model
132 best _ model <- results [ which .min ( results $ AIC ) , ]
133 print (" Best ARIMA Model based on AIC:")
134 print ( best _ model )
135
136 comparison _ table <- data . frame ( Model = c(" sarima _ model "," final _ model "
) ,
137 AIC = c( AIC ( sarima _ model ) , AIC ( final _ model ) ) ,
138 BIC = c( BIC ( sarima _ model ) , BIC ( final _ model ) ) ,
139 MSE = c( mean ( residuals ( sarima _ model ) ^2) ,mean ( residuals ( final _ model
) ^2) )
140 )
141
142 print ( comparison _ table )
143
144 # Fit the best model for further analysis
145 final _ model <- Arima ( data _ Generation , order = c(0 , 1 , 3) ,
146 seasonal = list ( order = c(0 , 1 , 1) , period = 12)
)
147 summary ( final _ model )
148 fitted _ values <- fitted ( final _ model )
149
150 observed _ data <- data . frame ( Time = as. numeric ( time ( data _ Generation ) ) ,
Values = as. numeric ( data _ Generation ) )
151 fitted _ data <- data . frame ( Time = as. numeric ( time ( data _ Generation ) ) ,
Values = as. numeric ( fitted _ values ) )
152
153 ggplot () +
154 geom _ line ( data = observed _data , aes ( x = Time , y = Values , color = "
Observed ") , linewidth = 1) +
155 geom _ line ( data = fitted _data , aes ( x = Time , y = Values , color = "
Fitted ") , linewidth = 1 , linetype = " dashed ") +
156 scale _ color _ manual ( name = " Series ", values = c(" Observed " = " blue ",
" Fitted " = "red") ) +
157 ggtitle (" Original data vs fittes data ") +
158 xlab (" Month ") +
159 ylab (" Values ") +
160 theme _ minimal ()
161
162 # ###########################
163 forecasted <- forecast ( final _model , h = 12)
164 plot ( forecasted )
165
166 # Extract the forecast data set
167 forecast _ data <- data . frame (
168 Time = time ( forecasted $ mean ) , # Time periods for
forecasts
169 Forecast = as. numeric ( forecasted $ mean ) , # Point forecasts
170 Lower _80 = forecasted $ lower [ , 1] , # 80% lower bound
171 Upper _80 = forecasted $ upper [ , 1] , # 80% upper bound
172 Lower _95 = forecasted $ lower [ , 2] , # 95% lower bound
173 Upper _95 = forecasted $ upper [ , 2] # 95% upper bound
174 )
175 forecast _ data
176
177 plot ( forecasted , main = " Time Series with SARIMA and Forecast ", xlab
= " Month ", ylab = " Values ", col = " blue ")
178 lines ( data _ Generation , col = " black ", lwd = 2)
179 lines ( fitted _values , col = "red", lwd = 2 , lty =2)
180 legend (" topleft ", legend = c(" Original Data ", " Fitted ( SARIMA )", "
Forecast ") ,
181 col = c(" black ", "red", " blue ") , lty = c(1 ,2 ,1) , lwd = 2)
182 grid ()
183
184 # View the forecast data set
185 print ( forecast _ data )
186
187 # residual check
188 checkresiduals ( final _ model $ residuals )
189 qqnorm ( final _ model $ residuals )
190 qqline ( final _ model $ residuals ,col ="red ")
191 normility _ check _ residual <- shapiro . test ( final _ model $ residuals )
192 if( normility _ check _ residual $p . value <= 0.05) print (" Residuals are not
normally distributed ") else print (" Residuals are normally
distributed ")

