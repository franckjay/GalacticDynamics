cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Author: Jay Franck  Email: franckjay@gmail.com
cc Course:             Instructor: 
cc
cc Program Name: luminosity function
cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Program Notes:
cc
cc
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

	program lumfunc

	implicit none

c.......INPUTS........................................................
	
c.......OUTPUT........................................................
	
c.......INTERMEDIATE..................................................

	double precision Mbol,m,d,phi,N,a,Pi

	integer i,j
	open(unit=2, file='m10.dat')
	open(unit=3, file='m25.dat')

	Pi = 2. * dasin(1.d0)

	Mbol= -6.


	do i= 1,25

		phi = 0.01*((Mbol+7.)**3)
		a=(-(Mbol-15.)/5.)
		N= phi*(4./3.)*Pi*(10.**a)**3
		write(2,*) N,'     ',phi
		Mbol=Mbol+1.

	enddo
c	print *,Mbol

	Mbol= -6.
	do i= 1,25

		phi = 0.01*((Mbol+7.)**3)
		a=(-(Mbol-30.)/5.)
		N= phi*(4./3.)*Pi*(10.**a)**3
		write(3,*) N,'     ',phi
		Mbol=Mbol+1.
	enddo







	return
	end



