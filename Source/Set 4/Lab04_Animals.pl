:-['Lab04_RULES_LIBRARY.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */
/*------------------------------------------------------------------
   EXAMPLE RULES FOR CLASSIFICATION OF ANIMALS
------------------------------------------------------------------*/

if      has_hair(X) or 
	gives_milk(X) 
then    animal(X,mammal).

if      has_feathers(X)  
then    animal(X,bird).

if      flies(X) and 
	lays_eggs(X) 
then    animal(X,bird).

if      animal(X,mammal) and 
	eats_meat(X) 
then    animal(X,carnivore).
	
if      animal(X,mammal) and 
	has_pointed_teeth(X) and 
	has_claws(X) and
	has_forward_pointed_eyes(X)
then    animal(X,carnivore).

if      animal(X,carnivore) and 
	has_tawny_colour(X) and
	has_dark_spots(X)
then    animal(X,cheetah).

if      animal(X,carnivore) and 
	has_tawny_colour(X) and
	has_black_stripes(X)
then    animal(X,tiger).

if      animal(X,bird) and
	not(flies(X)) and
	swims(X)
then    animal(X,penguin).

if      animal(X,bird) and
	good_flyer(X)
then    animal(X,albatros).


/*------------------------------------------------------------------
  ?-prove_bc(animal(petros,albatros)).
------------------------------------------------------------------*/
flies(petros).
lays_eggs(petros).
good_flyer(petros).


/*------------------------------------------------------------------
  ?-prove_bc(animal(jimmy,tiger)).
------------------------------------------------------------------*/

has_hair(jimmy).
has_pointed_teeth(jimmy).
has_claws(jimmy).
has_forward_pointed_eyes(jimmy).
has_black_stripes(jimmy).
has_tawny_colour(jimmy).
