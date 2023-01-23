%The following is a script to extract sustained velocity from the
%surface-skimming of Lednia stoneflies. 10/11/2019 - Anthony Lapsansky

%First, import the x-y points that correspond to the skimming of a specific 
%stonefly. These files contain the animal position (head and tail) from DLC
%plus two points marked on the behavioral tray to calibrating pixels to meters.

%The naming scheme is RearingTemp-Stream-IndividualID-SkimmingTrial

A1 = readtable('1C_Lunch_0013_Trial2.csv');
A2 = readtable("1C_Lunch_0013_Trial3.csv");
A3 = readtable("1C_Siyeh_0023_Trial1.csv");
A4 = readtable("1C_Siyeh_0023_Trial2.csv");
A5 = readtable("1C_Siyeh_0023_Trial3.csv");
A6 = readtable("1C_Siyeh_0024_Trial1.csv");
A7 = readtable("1C_Siyeh_0024_Trial2.csv");
A8 = readtable("1C_Siyeh_0024_Trial3.csv");
A9 = readtable("4C_Reynolds_0022_Trial1.csv");
A10 = readtable("4C_Reynolds_0022_Trial2.csv");
A11 = readtable("4C_Reynolds_0022_Trial3.csv");
A12 = readtable("4C_Reynolds_0030_Trial2.csv");
A13 = readtable("4C_Reynolds_0041_Trial1.csv");
A14 = readtable("4C_SextonMain_0031_Trial1.csv");
A15 = readtable("7C_Lunch_0020_Trial1.csv");
A16 = readtable("7C_Lunch_0020_Trial2.csv");
A17 = readtable("7C_Lunch_0033_Trial1.csv");
A18 = readtable("7C_Reynolds_0010_Trial1.csv");
A19 = readtable("7C_Reynolds_0010_Trial2.csv");
A20 = readtable("7C_Reynolds_0029_Trial1.csv");
A21 = readtable("7C_Reynolds_0029_Trial2.csv");
A22 = readtable("7C_Reynolds_0029_Trial3.csv");
A23 = readtable("7C_Reynolds_0029_Trial4.csv");
A24 = readtable("7C_Siyeh_0001_Trial1.csv");
A25 = readtable("7C_Siyeh_0001_Trial2.csv");
A26 = readtable("7C_Siyeh_0001_Trial3.csv");
A27 = readtable("7C_UnmarkedSexton_0012_Trial1.csv");
A28 = readtable("7C_UnmarkedSexton_0012_Trial4.csv");
A29 = readtable("7C_UnmarkedSexton_0028_Trial1.csv");
A30 = readtable("7C_UnmarkedSexton_0028_Trial2.csv");
A31 = readtable("7C_UnmarkedSexton_0028_Trial3.csv");
A32 = readtable("13C_UnmarkedSexton_0004_Trial1.csv");
A33 = readtable("Variable_Lunch_0027_Trial1.csv");
A34 = readtable("Variable_Lunch_0027_Trial2.csv");
A35 = readtable("Variable_Lunch_0027_Trial3.csv");
A36 = readtable("Variable_SextonMain_0025_Trial1.csv");
A37 = readtable("Variable_SextonMain_0025_Trial2.csv");
A38 = readtable("Variable_SextonMain_0025_Trial3.csv");

% Then, create a variable called A with the specific run that you wish to analyze
A = A38;

%Create column names for easier use later on
A.Properties.VariableNames = {'Frame','Head_X','Head_Y', 'Head_Likelihood', 'Back_X', 'Back_Y', 'Back_Likelihood', 'scale1X', 'scale1Y','scale2X','scale2Y'}; %just renaming the 

length_factor = sqrt((A.scale1X(1:1)-A.scale2X(1:1))^2+(A.scale1Y(1:1)-A.scale2Y(1:1))^2); %calculates the distance between the two dots on the pan
pixels_to_m = length_factor/0.1; %this converts pixels to distance (meters) given that the dots are 10 cm apart

A.X = A.Head_X/pixels_to_m; %converts X points to meters
A.Y = A.Head_Y/pixels_to_m ;%converts Y points to meters

X = A.X;
Y = A.Y;

% smooth the raw data, otherwise erroneously high velocities will result
% due to marker jitter. This is standard for kinematics anaylsis at high
% fps.
count = (1:size(A.X))';
X_smoothed = fit(count,A.X,'smoothingspline','SmoothingParam', 0.00001);
Y_smoothed = fit(count,A.Y,'smoothingspline','SmoothingParam', 0.00001);
X = X_smoothed(count);
Y = Y_smoothed(count);

% Plot the path of the stonefly
% Note: The raw data has many linear chunks of data. These are what happens
% when DLC has low confidence (points jump). Smoothing occurs correctly
% over continuous stretches of real data. Only select real data for
% calculating sustained skimming speed.
figure(1)
plot(A.X,A.Y) 
ylim([0 0.40]);
xlim([0 0.30]);
hold on
plot(X,Y)
title('Path of stonefly')

%the following calculates the velocity of the stonefly through time
velocity_X = (1 : size(X)-1)'; %this just initializing the matrices that we will put the data in
velocity_Y = (1 : size(Y)-1)';

for j=1: size(X)-1
    velocity_X(j,:)= (X(j+1,1)-X(j,1))/0.0005; %this computes the velocity in the x in a stepwise fashion
    velocity_Y(j,:)= (Y(j+1,1)-Y(j,1))/0.0005; %this computes the velocity in the y in a stepwise fashion
end
speed = sqrt(velocity_X.^2+velocity_Y.^2); %calculates speed

% Plot velocity. 
figure(2) %plots speed
plot(velocity_X)
hold on
plot(velocity_Y)
plot(speed)
ylim([-0.5 0.5]);
xlim();
title('Speed of stonefly')
legend('X Velocity', 'Y Velocity', 'Speed')

% By looking at the speed plot, manually select the region (between
% indices) that corresponds to a sustained skimming bout. The indices below
% are for A38 - "Variable_SextonMain_0025_Trial3.csv"
index1 = 3030;
index2 = 4077;
meansInsideInterval = mean(speed(index1:index2))

% The mean, sustained speed of each run was transfered to another excel 
% sheet for analysis

