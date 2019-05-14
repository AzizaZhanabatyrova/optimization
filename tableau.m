function [ ] = tableau( xnVar,xbVar,xn,xb,B,N,cn,cb,bigmapplied,initialcost,type)
fprintf('========================================\nTABLEAU\n\n');

% Initial cost
x0 = initialcost;

% Row 0 of t and col 0 of t
col0=[sym('xcost'),xbVar];
row0 = [xbVar,xnVar];

t = [cb,cn,x0; B,N,xb];
[j,k]=size(t);
fprintf('\n1 iteration\n');
fprintf('------------\n\n');
disp(t);
iteration=2;

% Condition for iteration of tableau
num6=0;
for num5 = 1:k-1
    if t(1,num5)<0
        num6=num6+1;
    end
end

% Loop to iterate the tableau and achieve optimality
while num6 > 0
    % Var entering the basis
    col=0;
    num2=0;
    for num = (1):(k-1)
        if (num2>t(1, num))
            num2=t(1, num);
            col = num;
        end
    end

    % Var leaving the basis
    num4=inf;
    row = -1;
    
    for num = 2:j
        num3 = t(num,k) / t(num,col);
        if all(t(num,col)>0) && all(num3<num4)
            num4=num3;
            row = num;
        end
    end

    if (row == -1)
       fprintf('Warning: Problem is unbounded\n'); 
       return;
    end

    % Update the basis
    col0(row) = row0(col);

 
    %Pivoting
    pivot = t(row,col);


    % New line of row
    t(row,:) = t(row,:) / pivot;


    % Rest of lines
    for num = 1:j
        if num~=row
            t(num,:) = t(num,:) - (t(num,col)*t(row,:));
        end
    end
    fprintf('\n%d iteration\n',iteration);
    fprintf('------------\n\n');
    format short
    disp(t);
    iteration = iteration +1;

    %Check improvement in cost
    if t(1,k)>x0
        fprintf('\nThe cost has improved: %.2f > %.2f \n\n', t(1,k),x0);
    else
            fprintf('The cost has not improved');
    end
    x0 = t(1,k);

    % Condition for iteration of tableau
    num6=0;
    for num5 = 1:k-1
        if t(1,num5)<0
            num6=num6+1;
        end
    end
end

col0 = col0(2:j);
xb = t(2:j,k);

xstarVal = [];
xstarVar = [];
x = sym('x', [1 size(col0,2)]);

for num7 = 1:size(col0,2)
    for i = 1:size(col0,2)
        if isequal(col0(i), x(num7))
            xstarVal = [xstarVal, xb(i)];
            xstarVar = [xstarVar, col0(i)];
        end
    end
end

unfeasible = 0;

if (bigmapplied == 1)
    fprintf('Big M has been applied to the problem.\n');
    m = size(t,1) -1;
    sbigm = sym('s', [1 2*m]);
    sbigm = sbigm(m+1:2*m);
    for num7 = 1:size(col0,2)
        for i = 1:size(col0,2)
            if isequal(col0(i), sbigm(num7))
                unfeasible = unfeasible + 1;
            end
        end
    end
end

unfeasible = 0;


if (unfeasible == 0)
    fprintf('========================================\n');
    fprintf('THE FINAL SOLUTION: \n\n');
    for num7 = 1:size(xstarVal,2)
        fprintf('%s = %.2f \n', xstarVar(num7), xstarVal(num7));
    end
    if all(type == 'min') || all(type == 'Min') || all(type == 'MIN')
       fprintf('xO = %.2f (Minimum problem) \n', -x0);
    else
    fprintf('xO = %.2f \n', x0);
    end
else
   fprintf('Warning: Problem is unfeasible\n'); 
end



end