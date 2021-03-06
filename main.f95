program plouff_solver
	use mod_functions
	implicit none
	
	type(polygon), allocatable, dimension(:) :: poly
	type(control_struct) :: configurations 
	
	call read_config(configurations)
	call create_sphere(configurations,poly)
    call calc_profil_vector(configurations)
    call profil_1d_x(configurations,poly)
     
    !call output_one_prism(configurations,poly)
	!call output_sphere(configurations,poly)
	!call test_numberOfEdges(configurations)
    !call test_numberOfLayers(configurations)

end program plouff_solver
