function [] = simplexsolver( A, b, c, type, sign )
tic;

% Standardize the problem 
[A,b,c,x,slack] = standardization(A, b, c, type, sign);

[m, n] = size(A);

% Choose a method of initialization
fprintf('========================================\nINITIALIZATION\n\n');
if (slack == m)
    disp('There is an identity matrix in Slack variables')
    B = eye(m);
    xnVar =  x(1:n-m);
    xbVar =  x(n-m+1:n);
    N = A(:, 1:n-m);
    xn(n-m, 1) = 0;
    xb = b;
    cn = c(1, 1:n-m)*-1;
    cb(m) = 0;
    bigmapplied = 0;
    initialcost = 0;
else
    disp('Using bigM');
    [xnVar,xbVar,xn,xb,B,N,cn,cb,initialcost] = bigm(A,b,c);
    bigmapplied = 1;
end
% Now we have B, N, xb, xn, and initial basic feasible solution



fprintf('\nBasic variables = ');
disp(xbVar);
fprintf('Nonbasic variables = ');
disp(xnVar);
disp('xb = ');
disp(xb);
disp('xn = ');
disp(xn);
disp('B = ');
disp(B);
disp('N = ');
disp(N);
fprintf('Coefficients of Basic var = ');
disp(cb);
fprintf('Coefficients of Non-basic var = ');
disp(cn);


% Call Tableau function
%[ output_args ] = 
tableau(xnVar,xbVar,xn,xb,B,N,cn,cb,bigmapplied,initialcost,type);


toc
end

