cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Author: Jay Franck  Email: franckjay@gmail.com
cc Course:  ASTR620    Instructor: Dr. Orosz
cc
cc Program Name: Gal Dyn Orbits-Problem 5: RK4 Box Orbits
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

	program boxRK4

	implicit none

c.......INPUTS........................................................
	double precision a,e,r,v,dt,x,y,dumx,dumy,vx,vy
	double precision kx(4),kvx(4),ky(4),kvy(4),dumVx,dumVy

c.......OUTPUT........................................................
	
c.......INTERMEDIATE..................................................
	integer i,j,N


	open(unit=1,file='loop.dat')

	


c.......Main.Program..................................................

c.......INITIAL.VALUES................................................
c.......BOX.ORBIT.............
c	N=100000
c	dt=.001
c	x=1.
c	y=0.0
c	vx=0.0
c	vy=.0001

c.......LOOP....................
	N=1000000
	dt=.01
	x=30.
	y=0.0
	vx=0.0
	vy=1.0



	write(1,*)x,'     ',y
	

c..............Get some new values..................................
	do i=1,N		


		dumx=x
		dumy=y

		call fun(dumx,dumx,dumy,dumVx)
		call fun(dumy,dumx,dumy,dumVy)

		kx(1)=(vx)*dt
		ky(1)=(vy)*dt

c		The Y-Value is divided by this value to represent 'q'
		kvx(1)=dumVx*dt
		kvy(1)=dumVy*dt/(0.9**2)


c...............k2........................	

		dumx=x+0.5*kx(1)
		dumy=y+0.5*ky(1)

		call fun(dumx,dumx,dumy,dumVx)
		call fun(dumy,dumx,dumy,dumVy)

		kx(2)=(vx+0.5*kvx(1))*dt
		ky(2)=(vy+0.5*kvy(1))*dt
		kvx(2)=dumVx*dt
		kvy(2)=dumVy*dt/(0.9**2)
c...............k3........................

		dumx=x+0.5*kx(2)
		dumy=y+0.5*ky(2)

		call fun(dumx,dumx,dumy,dumVx)
		call fun(dumy,dumx,dumy,dumVy)

		kx(3)=(vx+0.5*kvx(2))*dt
		ky(3)=(vy+0.5*kvy(2))*dt
		kvx(3)=dumVx*dt
		kvy(3)=dumVy*dt/(0.9**2)


c...............k4........................

		dumx=x+kx(3)
		dumy=y+ky(3)

		call fun(dumx,dumx,dumy,dumVx)
		call fun(dumy,dumx,dumy,dumVy)

		kx(4)=(vx+kvx(3))*dt
		ky(4)=(vy+kvy(3))*dt
		kvx(4)=dumVx*dt
		kvy(4)=dumVy*dt/(0.9**2)

		x=x+(1.0/6.0)*(kx(1)+2*kx(2)+2*kx(3)+kx(4))
		y=y+(1.0/6.0)*(ky(1)+2*ky(2)+2*ky(3)+ky(4))		
		vx=vx+(1.0/6.0)*(kvx(1)+2*kvx(2)+2*kvx(3)+kvx(4))
		vy=vy+(1.0/6.0)*(kvy(1)+2*kvy(2)+2*kvy(3)+kvy(4))
		
		

		write(1,*)x,'     ',y
				
	enddo



	close(1)
	return
	end



	subroutine fun(p1,dumx,dumy,dphi)

	double precision p1,rad,dphi,dumx,dumy



c	ISOCHRONE Potential
c	dphi = -rad*p1/(((9.+rad**2)**0.5)*((3+sqrt(9+rad**2))**2))
	
c	Box/Loop Potential
	dphi=-0.5*2.*p1/((0.14**2)+(dumx**2)+((dumy/0.9)**2))


	return
	end
