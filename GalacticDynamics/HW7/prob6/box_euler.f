cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Author: Jay Franck  Email: franckjay@gmail.com
cc Course: ASTR620    Instructor: Dr. Orosz
cc
cc Program Name: Gal Dyn Orbits- HW7 Problem 6: Eulers
cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



	program euler_box

	implicit none

c.......INPUTS........................................................
	double precision r,v,dt,x,y,dumx,dumy,vx,vy
	double precision dumVx,dumVy

c.......OUTPUT........................................................
	
c.......INTERMEDIATE..................................................
	integer i,j,N,flag


	open(unit=1,file='euler_L5.dat')

	


c.......Main.Program..................................................

c.......INITIAL.VALUES................................................
c.......L1.............
c	N=10000
c	dt=.001
c	x=0.
c	y=-0.9968
c	vx=0.0001
c	vy=0.0001
c.......L3.............
c	N=10000
c	dt=.001
c	x=0.
c	y=0.0
c	vx=0.0001
c	vy=0.0001
c.......L5............
	N=10000
	dt=.001
	x=-0.9949
	y=0.0
	vx=0.0001
	vy=0.0001

	write(1,*)x,'     ',y
	

c..............Get some new values..................................
	do i=1,N		


		dumx=x
		dumy=y

		x=dumx+vx*dt
		y=dumy+vy*dt	

		call fun(dumx,dumx,dumy,dumVx,1)
		call fun(dumy,dumx,dumy,dumVy,3)

		vx=vx+dumVx*dt
		vy=vy+dumVy*dt		

		

		write(1,*)x,'     ',y
				
	enddo





	close(1)
	return
	end



	subroutine fun(p1,dumx,dumy,dphi,flag)

	double precision p1,rad,dphi,dumx,dumy
	integer flag

c	Radius**2
	rad=(dumx**2)+(dumy**2)

c	ISOCHRONE Potential
c	dphi = -rad*p1/(((9.+rad**2)**0.5)*((3+sqrt(9+rad**2))**2))
	
c	Box/Loop Potential
	dphi=0.5*2.*p1/((0.1**2)+(dumx**2)+((dumy/0.8)**2))

c	Divide by q**2 if this is dphi/dy
	if (flag .EQ. 3) then
		dphi=dphi/(0.8**2)
	endif

c	Rotating Box
	dphi=-(dphi-p1)

	return
	end
