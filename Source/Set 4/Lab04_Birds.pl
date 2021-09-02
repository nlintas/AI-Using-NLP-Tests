:-['Lab04_SemanticNet_LIBRARY'].
/** Author: Nikolaos Lintas from University of Sheffield */

isa(bird,animal).
isa(canary,bird).
isa(penguin,bird).

instance_of(tweety,canary).
instance_of(tux,penguin).

hasproperty(animal,breathes,oxygen).
hasproperty(bird,travel,fly).
hasproperty(bird,reproduction,eggs).
hasproperty(canary,colour,yellow).
hasproperty(penguin,travel,walk).
hasproperty(penguin,colour,black_white).

