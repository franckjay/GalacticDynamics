cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Author: Jay Franck  Email: franckjay@gmail.com
cc Course: ASTR620    Instructor: Dr. Orosz
cc
cc Program Name: Gal Dyn Orbits- Problem 4: Eulers
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

	program euler

	implicit none

c.......INPUTS........................................................
	double precision r,v,dt,x,y,dumx,dumy,G,M,vx,vy
	double precision dumVx,dumVy

c.......OUTPUT........................................................
	
c.......INTERMEDIATE..................................................
	integer i,j,N


	open(unit=1,file='euler.dat')

	


c.......Main.Program..................................................

c.......INITIAL.VALUES................................................
	N=1000000
	G=1.0
	M=1.0
		
	dt=.003

	x=5.
	y=0.0

	vx=0.0
	vy=.01
	write(1,*)x,'     ',y
	

c..............Get some new values..................................
	do i=1,N		


		dumx=x
		dumy=y

		x=dumx+vx*dt
		y=dumy+vy*dt	

		call fun(dumx,dumx,dumy,G,M,dumVx)
		call fun(dumy,dumx,dumy,G,M,dumVy)

		vx=vx+dumVx*dt
		vy=vy+dumVy*dt		

		

		write(1,*)x,'     ',y
				
	enddo





	close(1)
	return
	end



	subroutine fun(p1,dumx,dumy,G,M,dphi)

	double precision p1,rad,G,M,dphi,dumx,dumy

	rad=((dumx**2)+(dumy**2))

	dphi = -rad*p1/(((9.+rad**2)**0.5)*((3+sqrt(9+rad**2))**2))

	return
	end
