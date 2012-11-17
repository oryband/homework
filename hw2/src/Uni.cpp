#include "../include/Uni.h"

Uni :: Uni(bool flag) {

	Department Cs,Pg,El;


//put here all the headach from uni.cpp hw1 with new and delete for reading files







	if (flag) { // according to plan


	}
	else { // PG department don't register and prints to file


	}

}
//for reading files without risking the Departmets elements
Department& Uni :: getCs()const{
				return Cs;
}
Department& Uni :: getPg()const{
				return Pg;
}
Department& Uni :: getEl()const{
				return El;
}
