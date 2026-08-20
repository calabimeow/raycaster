all:
	gfortran raycaster.f90 -o raycaster -L. -lraylib
	./raycaster
