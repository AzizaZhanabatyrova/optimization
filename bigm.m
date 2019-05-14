function [xnVar,xbVar,xn,xb,B,N,cn,cb,initialcost] = bigm(A,b,c)

    [m, n] = size(A);
    B = eye(m);
    N = A;
    
    
    % Big M
    M = max(max(A)) + max(b) + max(c);
    M = 4;
    
    % Non basic cost vector
    cn = [c(1, 1:n-m)*-1 (zeros(1,m))] - M*sum(A);

    % Basic cost vector
    cb  = (zeros(1,m));
    
    
    % Defining symbolic basic and non basic variables
    xnVar = [sym('x', [1 (n - m)]) sym('s', [1 m])];
    xbVar = sym('s', [1 2*m]);
    xbVar = xbVar(m+1:2*m);
    
    % Defining initial values for basic and non basic variables
    xn = zeros(1,n)';
    xb = b;
    
    initialcost = -M*sum(b);


end

