/*/* Examples for testing */
/*
x/;
x;
y/;

lambda x. x;
(lambda x. x) (lambda x. x x); 
/*(lambda x. x x) (lambda x. x x); */
x/;
lambda x. x;
x;
((lambda x . x x) (lambda y . y));
((lambda x . x) y);
((lambda x . (x x)) ((lambda y . y) (lambda z . z)));
*/
((lambda z . (lambda x . z x)) z);
*/
(lambda f . lambda x . f (f (f x))) (lambda f . lambda x . f (f x));
