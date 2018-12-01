cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc Author: Jay Franck  Email: franckjay@gmail.com
cc Course: Galactic Dynamics   Instructor: Dr. Orosz
cc
cc Program Name: Potential/Density/Vcirc Plotting
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

	program potential

	implicit none

	integer i
	double precision rho,vel,pot,r,pi,a,c

c.......INPUTS........................................................
	
c.......OUTPUT........................................................
	
c.......INTERMEDIATE..................................................
	
	pi = 2. * dasin(1.d0)

	open(unit=1, file='i_pot.dat')
	open(unit=2, file='i_rho.dat')
	open(unit=3, file='i_vel.dat')

	open(unit=4, file='ii_pot.dat')
	open(unit=5, file='ii_rho.dat')
	open(unit=6, file='ii_vel.dat')

	open(unit=7, file='iii_pot.dat')
	open(unit=8, file='iii_rho.dat')
	open(unit=9, file='iii_vel.dat')

	open(unit=10, file='iv_pot.dat')
	open(unit=11, file='iv_rho.dat')
	open(unit=12, file='iv_vel.dat')

	open(unit=13, file='v_pot.dat')
	open(unit=14, file='v_rho.dat')
	open(unit=15, file='v_vel.dat')

	open(unit=16, file='vi_pot.dat')
	open(unit=17, file='vi_rho.dat')
	open(unit=18, file='vi_vel.dat')



	r= 0.0001
	a=0.5

	do i=1, 10000
	
c...............Part i
			
		pot= -1./r

		rho= 0.0
		
		vel= sqrt(1./r)

		write(1,*) r, '   ', pot
		write(2,*) r, '   ', rho
		write(3,*) r, '   ', vel

c...............Part ii
		pot= (-3./(2.*a**3))*((a**2)-(r*r/3.))

		rho= 3./(4.*pi*a**3)
c		Normalized
		rho=1.0
		
		vel= r*a**1.5

		if (r .GT. a) then
			vel= sqrt(1./r)
		endif

		write(4,*) r, '   ', pot
		write(5,*) r, '   ', rho
		write(6,*) r, '   ', vel			

c...............Part iii
		pot= 4.*pi*log(r)

c		Normalize rho=1, r=1
		rho=1/r**2
		
		vel= sqrt(4*pi)

		write(7,*) r, '   ', pot
		write(8,*) r, '   ', rho
		write(9,*) r, '   ', vel

c...............Part iv
		pot= -1./(((r**2)+(a**2))**1.5)

		rho=(3./4./pi)*((1./(((r**2)+(a**2))**1.5))-
     -		 (r**2/(((r**2)+(a**2))**2.5)))
c		Normalize rho=1, r=1
c		Found by finding rho(r=1), then multiplying that
c		by a constant to get it to c*rho(r=1)=1.
		rho=rho*29.27
		
		vel= (r**2)/(((r**2)+(a**2))**1.5)

		write(10,*) r, '   ', pot
		write(11,*) r, '   ', rho
		write(12,*) r, '   ', vel

c...............Part v
		pot= -1./(r+a)

		rho= 1./(((r+a)**2)*((1./r)-(1/(r+a))))
c		Normalize rho=1, r=1

		rho=rho*0.75
		vel= sqrt(r/((r+a)**2))

		write(13,*) r, '   ', pot
		write(14,*) r, '   ', rho
		write(15,*) r, '   ', vel

c...............Part vi
		pot=(1./a)*log(r/(r+a))

		rho=(1./(1.))*((1/r**2)-(2/(r**2 +r*a))+
     +           (1./((r+a)**2)))
c		Normalize rho=1, r=1

		rho=rho*9.0
		
		vel= sqrt(1.-(r/(r+a)))

		write(16,*) r, '   ', pot
		write(17,*) r, '   ', rho
		write(18,*) r, '   ', vel

		r= 0.0001 + r
	enddo







	close(1)
	close(2)
	close(3)

	close(4)
	close(5)
	close(6)

	close(7)
	close(8)
	close(9)

	close(10)
	close(11)
	close(12)

	close(13)
	close(14)
	close(15)

	close(16)
	close(17)
	close(18)


	return
	end
