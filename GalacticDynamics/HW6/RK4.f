cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Author: Jay Franck  Email: franckjay@gmail.com
cc Course:             Instructor: 
cc
cc Program Name: Gal Dyn Orbits
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

	program RKorbits

	implicit none

c.......INPUTS........................................................
	double precision a,e,r,v,dt,x,y,dumx,dumy,G,M,vx,vy
	double precision kx(4),kvx(4),ky(4),kvy(4),dumVx,dumVy

c.......OUTPUT........................................................
	
c.......INTERMEDIATE..................................................
	integer i,j,N


	open(unit=1,file='RK_e9.dat')

	


c.......Main.Program..................................................

c.......INITIAL.VALUES................................................
	N=1000000
	G=1.0
	M=1.0
	a = 10.
	e=0.1	
	dt=.001
c	At periastron
	r = a*(1.-e)
	v = sqrt((2./r)-(1./a))
	
	x=r
	y=0.0

	vx=0.0
	vy=v
	

c..............Get some new values..................................
	do i=1,N		


		dumx=x
		dumy=y

		call fun(dumx,dumx,dumy,G,M,dumVx)
		call fun(dumy,dumx,dumy,G,M,dumVy)

		kx(1)=(vx)*dt
		ky(1)=(vy)*dt

		kvx(1)=dumVx*dt
		kvy(1)=dumVy*dt


c...............k2........................	

		dumx=x+0.5*kx(1)
		dumy=y+0.5*ky(1)

		call fun(dumx,dumx,dumy,G,M,dumVx)
		call fun(dumy,dumx,dumy,G,M,dumVy)

		kx(2)=(vx+0.5*kvx(1))*dt
		ky(2)=(vy+0.5*kvy(1))*dt
		kvx(2)=dumVx*dt
		kvy(2)=dumVy*dt
c...............k3........................	


		dumx=x+0.5*kx(2)
		dumy=y+0.5*ky(2)

		call fun(dumx,dumx,dumy,G,M,dumVx)
		call fun(dumy,dumx,dumy,G,M,dumVy)

		kx(3)=(vx+0.5*kvx(2))*dt
		ky(3)=(vy+0.5*kvy(2))*dt
		kvx(3)=dumVx*dt
		kvy(3)=dumVy*dt


c...............k4........................	


		dumx=x+kx(3)
		dumy=y+ky(3)

		call fun(dumx,dumx,dumy,G,M,dumVx)
		call fun(dumy,dumx,dumy,G,M,dumVy)

		kx(4)=(vx+kvx(3))*dt
		ky(4)=(vy+kvy(3))*dt
		kvx(4)=dumVx*dt
		kvy(4)=dumVy*dt




		x=x+(1.0/6.0)*(kx(1)+2*kx(2)+2*kx(3)+kx(4))
		y=y+(1.0/6.0)*(ky(1)+2*ky(2)+2*ky(3)+ky(4))		
		vx=vx+(1.0/6.0)*(kvx(1)+2*kvx(2)+2*kvx(3)+kvx(4))
		vy=vy+(1.0/6.0)*(kvy(1)+2*kvy(2)+2*kvy(3)+kvy(4))
		
		

		write(1,*)x,'     ',y

c...............TEST FOR CLOSED ORBIT...........................
		
		if ((r**2)-((x**2)+(y**2)) .LE. 1.E-6) then
			print *,"Orbit is closed"
		endif
				
	enddo





	close(1)
	return
	end



	subroutine fun(p1,dumx,dumy,G,M,dv)

	double precision p1,rad,G,M,dv,dumx,dumy

	rad=((dumx**2)+(dumy**2))

	dv = -(G*M*p1)/(rad**1.5)

	return
	end
