

#### nonvac doesn't change the behaviour, vaccinated individuals go to a higher level, but oscilates with nonvac, waning immunity both vaccine and rec 46 32
dc = [1;map(y-> 92+y,0:9);map(y->127+y,0:14);map(y->170+y,0:9);map(y->205+y,0:40);map(y->290+y,0:14);map(y->316+y,0:43);map(y->392+y,0:12);map(y->472+y,0:26)]
rc = [1.0;map(y-> 1.0-(0.09/10)*y,1:10);map(y-> 0.91-(0.145/15)*y,1:15);map(y-> 0.765+(0.02/10)*y,1:10);map(y-> 0.785-(0.12/41)*y,1:41);map(y-> 0.665+(0.27/15)*y,1:15);map(y-> 0.935-(0.21/44)*y,1:44);map(y-> 0.725-(0.08/13)*y,1:13);map(y-> 0.645-(0.39536/27)*y,1:27)]
run_param_scen_cal(true,0.121,"newyorkcity",15,1,1,1,1,1,1,rc,dc,528,true,425,80) 
run_param_scen_cal(true,0.121,"newyorkcity",15,1,1,1,1,1,2,rc,dc,528,false,425,80) 

