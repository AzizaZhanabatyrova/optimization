function [A, b, c, x, slack] = standardization(A, b, c, type, sign)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    % Changing to standard form
                     
[m,n] = size(A);

% Check type, change from min to max
if all(type == 'min') || all(type == 'Min') || all(type == 'MIN')
    c = c * -1;
    type = 'max';
elseif all(type == 'max') || all(type == 'Max') || all(type == 'MAX')
    ;
else
    error('Error. Enter a type of a proper format.')
end



% Check dimensions of A and c
if (size(c,2) ~= n)
    error('Error. The dimension of vector c or matrix A is wrong.')
end



% Check dimensions of vector b
if (size(b,1) ~= m)
    error('Error. The number of elements in vector b is wrong.')
end



% Check dimensions of array "Sign" 
if (size(sign, 2) ~= m)
    error('Error. The number of elements in array Sign is wrong.')
end



% Check format of array "Sign" 
for num = 1:m
    if all(strcmp(sign(num), '<=')) || all(strcmp(sign(num), '>='))
        ;
    else
        error('Error. The format of some elements in array Sign is wrong.')
    end
end



%change b vector
sign2 = {'<=', '>='};
for num = 1:m
    if (b(num) < 0)
        b(num)=b(num)*-1;
        
        %change A for b
        A(num,:)= A(num,:)*-1;
        
        %change inequalities for b     
        if strcmp(sign(num), '<=')
            sign(num) = sign2(2);
        else
            sign(num) = sign2(1);
        end
    end
end 



% Change A for slack and use in the next part
slack = 0;
for num = 1:m
    if strcmp(sign(num), '<=')
        newcol(m, 1)= 0;
        newcol(num)=1;
        A = [A(:,1:n) newcol];
        newcol(num)=0;
        n = n+1;
        slack = slack+1;
    else 
        newcol(m, 1)= 0;
        newcol(num)= -1;
        A = [A(:,1:n) newcol];
        newcol(num)=0;
        n = n+1;
    end
end
% Now inequalities are changed to equalities



disp('=======================================')
disp('STANDARDIZED FORM ');
disp(' ');
disp('Type = ');
disp(type);
disp(' ')
disp('A = ');
disp(A);
disp('b = ');
disp(b);



% Change c vector
c(1, n)= 0;
disp('C = ')
disp(c);



%xs is a new vector of variables, including slack and surplus
r=0; t=0;
for num = 1:n
    if (c(num) == 0)
        r=r+1;
    else
        t=t+1;
    end
end

if (r > 0)
    x = [sym('x', [1 t]) , sym('s', [1 r]) ];
else
    x = sym('x', [1 sizeC(2)]);
end
disp('x = ');
disp(transpose(x));



% Check rank of A
if (rank(A) < m)
    error('Error. The matrix A has linearly dependent rows.')
end


% Check if m > n
if (n < m)
    error('Error. m>n, so no feasible solution exists.')
end

end