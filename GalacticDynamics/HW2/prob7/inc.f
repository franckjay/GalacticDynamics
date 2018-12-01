	program inc

	implicit none

	

	real a,b,q,d,tot,qtot,x,z
	real ran3
	integer i,iseed
	tot=0.0
	qtot=0.0

	
	iseed=-4

	do i=1,10000

		d= 0.0
		a=0.0
		b=0.0
		a=ran3(iseed)*90.
		b=ran3(iseed)*90.


		if (a .LE. b) then
			q=a/b
		endif
		
		if (b .LE. a) then
			q=(b/a)
		endif
		qtot= qtot + q
c		print *,q
		d=acos(q)*57.2957795
c		print *,d
		tot = tot+d

	enddo
	tot= tot/10000.

	print *,"Average inc: ",tot





		


	close(2)
	return
	end
