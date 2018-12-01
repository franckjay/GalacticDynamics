cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Author: Jay Franck  Email: franckjay@gmail.com
cc Course:  ASTR620    Instructor: Dr. Orosz
cc
cc Program Name: Gal Dyn Orbits-Problem 6: RK4 Rot. Pot Orbits
cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



	program rot_box

	implicit none

c.......INPUTS........................................................
	double precision a,e,r,v,dt,x,y,dumx,dumy,vx,vy
	double precision kx(4),kvx(4),ky(4),kvy(4),dumVx,dumVy

c.......OUTPUT........................................................
	
c.......INTERMEDIATE..................................................
	integer i,j,N


	open(unit=1,file='L1.dat')	


c.......Main.Program..................................................

c.......INITIAL.VALUES................................................

c.......L1.............
	N=10000
	dt=.001
	x=0.
	y=-0.9968
	vx=0.0001
	vy=0.0001
c.......L3.............
c	N=10000
c	dt=.001
c	x=0.
c	y=0.0
c	vx=0.0001
c	vy=0.0001
c.......L5............
c	N=10000
c	dt=.005
c	x=-0.9949
c	y=0.0
c	vx=0.0001
c	vy=0.0001




	write(1,*)x,'     ',y
	

c..............Get some new values..................................
	do i=1,N

		dumx=x
		dumy=y

		call fun(dumx,dumx,dumy,dumVx,1)
		call fun(dumy,dumx,dumy,dumVy,3)

		kx(1)=(vx)*dt
		ky(1)=(vy)*dt
		kvx(1)=dumVx*dt
		kvy(1)=dumVy*dt


c...............k2........................	

		dumx=x+0.5*kx(1)
		dumy=y+0.5*ky(1)

		call fun(dumx,dumx,dumy,dumVx,1)
		call fun(dumy,dumx,dumy,dumVy,3)

		kx(2)=(vx+0.5*kvx(1))*dt
		ky(2)=(vy+0.5*kvy(1))*dt
		kvx(2)=dumVx*dt
		kvy(2)=dumVy*dt
c...............k3........................

		dumx=x+0.5*kx(2)
		dumy=y+0.5*ky(2)

		call fun(dumx,dumx,dumy,dumVx,1)
		call fun(dumy,dumx,dumy,dumVy,3)

		kx(3)=(vx+0.5*kvx(2))*dt
		ky(3)=(vy+0.5*kvy(2))*dt
		kvx(3)=dumVx*dt
		kvy(3)=dumVy*dt


c...............k4........................

		dumx=x+kx(3)
		dumy=y+ky(3)

		call fun(dumx,dumx,dumy,dumVx,1)
		call fun(dumy,dumx,dumy,dumVy,3)

		kx(4)=(vx+kvx(3))*dt
		ky(4)=(vy+kvy(3))*dt
		kvx(4)=dumVx*dt
		kvy(4)=dumVy*dt

		x=x+(1.0/6.0)*(kx(1)+2*kx(2)+2*kx(3)+kx(4))
		y=y+(1.0/6.0)*(ky(1)+2*ky(2)+2*ky(3)+ky(4))		
		vx=vx+(1.0/6.0)*(kvx(1)+2*kvx(2)+2*kvx(3)+kvx(4))
		vy=vy+(1.0/6.0)*(kvy(1)+2*kvy(2)+2*kvy(3)+kvy(4))
		
		

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
