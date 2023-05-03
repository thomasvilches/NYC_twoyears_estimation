
dc = [1;map(y-> 92+y,0:9);map(y->129+y,0:14);map(y->170+y,0:9);map(y->205+y,0:40);map(y->290+y,0:14);map(y->316+y,0:43);map(y->392+y,0:12);map(y->472+y,0:26);map(y->527+y,0:12);map(y->618+y,0:9);map(y->641+y,0:9);map(y->680+y,0:9);map(y->730+y,0:49);map(y->831+y,0:14)]
rc = [1.0;map(y-> 1.0-(0.09/10)*y,1:10);map(y-> 0.91-(0.145/15)*y,1:15);map(y-> 0.765+(0.02/10)*y,1:10);map(y-> 0.785-(0.12/41)*y,1:41);map(y-> 0.665+(0.27/15)*y,1:15);map(y-> 0.935-(0.21/44)*y,1:44);map(y-> 0.725-(0.08/13)*y,1:13);map(y-> 0.645-(0.39536/27)*y,1:27);map(y-> 0.24964+(0.10436/13)*y,1:13);map(y-> 0.354-(0.054/10)*y,1:10);map(y-> 0.30+(0.035/10)*y,1:10);map(y-> 0.335-(0.045/10)*y,1:10);map(y-> 0.295+(0.025/50)*y,1:50);map(y-> 0.32-(0.025/15)*y,1:15)]

# calibrating
#run_param_scen_cal(true,0.121,"newyorkcity",15,1,1,1,1,1,0,2,rc,dc,[852;-1;-3;-4;-5;-7;-9],[0.0;0.0;0.0;0.0;0.0],true,425,80,0)

# vaccinate up to the end in the current pace March 31st
run_param_scen_cal(true,0.121,"newyorkcity",15,1,1,1,1,1,0,2,rc,dc,[1033;-1;-3;-4;-5;-7;-9],[0.0;0.0;0.0;0.0;0.0],1.0)

#vaccinate Jan, Feb, March
run_param_scen_cal(true,0.121,"newyorkcity",15,1,1,1,1,1,4,2,rc,dc,[852;852;941;1033;-5;-7;-9],[0.6;0.5;0.38;0.54;0.75],1.0)



dc = [1;map(y-> 92+y,0:9);map(y->129+y,0:14);map(y->170+y,0:9);map(y->205+y,0:40);map(y->290+y,0:14);map(y->316+y,0:43);map(y->392+y,0:12);map(y->472+y,0:26);map(y->527+y,0:12);map(y->618+y,0:9);map(y->641+y,0:9);map(y->680+y,0:9);map(y->730+y,0:49);map(y->831+y,0:14)]
rc = [1.0;map(y-> 1.0-(0.09/10)*y,1:10);map(y-> 0.91-(0.145/15)*y,1:15);map(y-> 0.765+(0.02/10)*y,1:10);map(y-> 0.785-(0.12/41)*y,1:41);map(y-> 0.665+(0.27/15)*y,1:15);map(y-> 0.935-(0.21/44)*y,1:44);map(y-> 0.725-(0.08/13)*y,1:13);map(y-> 0.645-(0.39536/27)*y,1:27);map(y-> 0.24964+(0.10436/13)*y,1:13);map(y-> 0.354-(0.054/10)*y,1:10);map(y-> 0.30+(0.035/10)*y,1:10);map(y-> 0.335-(0.045/10)*y,1:10);map(y-> 0.295+(0.015/50)*y,1:50);map(y-> 0.31-(0.017/15)*y,1:15)]

# vaccinate up to the end in the current pace March 31st
run_param_scen_cal(true,0.121,"newyorkcity",15,1,1,1,1,1,0,2,rc,dc,[1033;-1;-3;-4;-5;-7;-9],[0.0;0.0;0.0;0.0;0.0],0.5)

#vaccinate Jan, Feb, March
run_param_scen_cal(true,0.121,"newyorkcity",15,1,1,1,1,1,4,2,rc,dc,[852;852;941;1033;-5;-7;-9],[0.6;0.5;0.38;0.54;0.75],0.5)
