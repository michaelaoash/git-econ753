## For dataframe
## pkg install -forge dataframe
pkg load dataframe
## x = dataframe (rand (7, 3), "colnames", {"foo", "bar", "baz"}, "rownames", {"red","orange","yellow","green","blue","indigo","violet"})

## For xlsread
## pkg install -forge io
pkg load io

## Employment to output ratio 
[D,T,R,l] = xlsread ('I-O Analysis for Tutorial--02152023--SC.xlsx', 'E-O Ratio(commodities)') ;
eo = D(1:165,10)

## Requirement Matrix
[An, Tn, Ra, limits] = xlsread ('Input-Output tables_Producers price--Detailed Table--08202021.xlsx', 'Input coefficients') ;
industries = Tn(3:167,2)
size(An)
A = An(1:165,1:165) ;


Ad = dataframe(A, "colnames", industries, "rownames", industries) ;
eye(size(Ad)) - Ad
size(eye(size(Ad)) - Ad)
IMinusA = eye(size(Ad)) - Ad ;
LI = inv(IMinusA) ;
[LI, rcond] = inv( IMinusA) ;


Leontief.inverse = inv( eye(size(A)) - A ) ;

y = repmat(0, 1, 165)'
y(130) = 1

x = Leontief.inverse * y
employment =  eo .* x

