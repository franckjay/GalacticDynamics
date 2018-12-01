cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Author: Jay Franck  Email: franckjay@gmail.com
cc Course: Astro620            Instructor: Jerome Orosz
cc
cc Program Name: Brute
cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Program Notes:
cc
cc Brute force numerical method of solving a transcendental function
cc
cc
cc
cc VARIABLES
cc __________________________________________________________________
cc INPUT.............................................................
cc
cc
cc
cc OUTPUT............................................................
cc 
cc
cc
cc INTERMEDIATE......................................................
cc
cc
cc
cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	program bruteforce

	implicit none

c.......INPUTS........................................................
	double precision a,da,diff
	
	
c.......OUTPUT........................................................
	
c.......INTERMEDIATE..................................................
	integer i,j

	da=1.e-5


	do i=1,500000


		a = da*((exp(da))/(exp(da)-1))-5
		
		if (abs(a) .LT.	0.00001) then
			print *,da

		endif

		da = da + 1.e-5

	enddo


	return
	end


