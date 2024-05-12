% Goals
goal(g1). goal(g2). goal(g3). goal(g4). goal(g5).
goal(g6). goal(g7). goal(g8). goal(g9). goal(g10).
goal(g11). goal(g12). goal(g13). goal(g14).
goal(g15).

% Tasks
task(t1). task(t2). task(t3). task(t4). task(t5).
task(t6). task(t7). task(t8). task(t9). task(t10).
task(t11). task(t12). task(t13). task(t14). task(t15).
task(t16). task(t17). task(t18).
task(t19). task(t20).  task(t21).  task(t22).

% AND-nodes
and_node(g1).
and_node(g3).
and_node(g4).
and_node(g8).
and_node(g10).
and_node(g12).
and_node(g13).


or_node(g2).
or_node(g5).
or_node(g7).
or_node(g9).
or_node(g11).
or_node(g14).
or_node(g15).

% Parenthood
parent(g1,g2). parent(g1,g3). parent(g1,g4). parent(g1,g5).
parent(g2,t1). parent(g2,t2).
parent(g3,t3). parent(g3,t4). parent(g3,g7). parent(g3,g8).
parent(g4,g9). parent(g4,g10).
parent(g5,g11). parent(g5,g12).
parent(g7,t5). parent(g7,t6).
parent(g8,t7). parent(g8,t8).
parent(g9,t9). parent(g9,g13).
parent(g10,t11). parent(g10,t12).
parent(g11,t13). parent(g11,g15). parent(g11,t22).
parent(g12,t15). parent(g12,g14). 
parent(g13,t17). parent(g13,t18).
parent(g14,t19). parent(g14,t20).  parent(g14,t21). 
parent(g15,t14). parent(g15,t16). 

% Precedences
pre(g2,g3).
pre(g2,g4).
pre(g3,g5).
pre(g4,g5).
pre(t3,g8).
pre(t4,g8).
pre(t4,g7).
pre(g9,g8).
pre(g10,g9).
pre(g11,g12).
pre(g13,t8).
pre(t17,t6).


% Contributions
contr(t1,q1,plus).
contr(t5,q1,plus).
contr(g12,q1,plus).
contr(t14,q1,plus).

% Priorities
promote(q1).

% A foo dependency
dependency(foo,foo,foo,tos).


%
% Other - complementary
%

contr(a,interesting,plus).
promote(interesting).
promote(profitable).
contr(b,interesting,plus).
contr(b,profitable,minus).
contr(c,profitable,minus).
contr(d,interesting,plus).


feas(a,s0).
feas(c,s0).
feas(d,s0).

task(a).
task(b).
task(c).
task(f).




