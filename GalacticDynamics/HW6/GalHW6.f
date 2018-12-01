cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Author: Jay Franck  Email: franckjay@gmail.com
cc Course: ASTR620     Instructor: Dr. Orosz
cc
cc Program Name: Gal Dyn Orbits-Problems 1 + 2
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

	program orbits

	implicit none

c.......INPUTS........................................................
	double precision a,e,phi,r,phase,omega,x,y
	double precision bigX,bigY,sigX,sigY,t	
c.......OUTPUT........................................................
	
c.......INTERMEDIATE..................................................
	integer i,j


	open(unit=1,file='1iv.dat')
	open(unit=2,file='exp.dat')
	


c.......Main.Program..................................................
	
	
c	Part A
c	a=10.
c	e=0.1
c	phase=0.0

c	Part B
c	a=10.
c	e=0.5
c	phase=0.0


c	Part C
c	a=10.
c	e=0.5
c	phase=60.0

c	Part D
	a=10.
	e=0.9
	phase=200.0

	r=0.
	phi=0.

	do i=0,36000

		r=a*(1.-e**2)/(1+e*cos(phi-phase))

		x=r*cos(phi)
		y=r*sin(phi)

		write(1,*) x,'   ',y

		phi= phi + .001
	enddo
	


c.......PROBLEM...2....................

c	Part i
c	omega=4.0
c	bigX=10.0
c	bigY=10.0
c	sigX=0.0
c	sigY=0.0

c	Part ii
c	omega=4.0
c	bigX=10.0
c	bigY=10.0
c	sigX=0.0
c	sigY=60.0

c	Part iii
c	omega=6.0
c	bigX=10.0
c	bigY=10.0
c	sigX=0.0
c	sigY=0.0

c	Part iv
c	omega=4.0
c	bigX=10.0
c	bigY=2.0
c	sigX=10.0
c	sigY=10.0


	t=0.0

	do i=1,10000


		x = bigX*cos(omega*t+sigX)
		y = bigY*sin(omega*t+sigY)

		write(2,*) x,'   ',y

		t=t+0.01
	enddo



	close(1)
	close(2)
	return
	end
