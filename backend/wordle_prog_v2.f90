!October 8, 2022.  Program to help me save time looking up words for Wordle.
! I tell the program what feedback I've had from Wordle, and it tells me what
! possible words are left.  That's all.
!*********************************************************************************

PROGRAM wordle_help
IMPLICIT NONE

INTEGER i, j, k, l, count_wordle, count_wordle_used, try_count, idoc, idec
INTEGER count_NOT_HERE(5), count_NOT_ANYWHERE

INTEGER MBS_chk(100), count_MBS
CHARACTER*1 MBS(10), NOT_HERE(5,100), NOT_ANYWHERE(100)

CHARACTER*1 W(5000,5)
! W(5000,5) are the 2309 Wordle words, one character for each of the five letters.
! W(j,i)= the value of the letter in the i^th position in the j^th word.

CHARACTER*1 USED(5000,5)
! USED(5000,5) are the Wordle words that have been used so far in the game.

INTEGER IS_MUST(5)
CHARACTER*1 ML(5)
! IS_MUST(i)=1, means that a Must exists for Position i. 
! IS_MUST(i)=0, means that a Must does not exist for Letter i. 
! When IS_MUST(i)=1, then ML(i)= the value the letter must have for position i. 
! When IS_MUST(i)=0, then ML(i)=' '.  a blank character.

INTEGER POSSIBLE(5000)
! POSSIBLE(i)=1 if the i^th Wordle word is possible; o.w., it is zero.

!Read in all the wordle words
OPEN(unit=1,file='wordle_list_all.txt')

i=1
do 
	read(1,11,end=5) (W(i,j), j=1,5) 
        POSSIBLE(i)=1
! at first, all wordle words are possible to be the answer
11	format(5a1)
	i=i+1

enddo
5 continue
count_wordle = i-1
write(*,*) 'number of wordle words read in = ',count_wordle

CLOSE(1)


!Read in all the used wordle words
OPEN(unit=1,file='wordle_list_used.txt')

i=1
do 
	read(1,11,end=85) (USED(i,j), j=1,5) 
	i=i+1

enddo
85 continue
count_wordle_used = i
write(*,*) 'number of used wordle words read in = ',count_wordle_used

!I think the next thing to do is to simply have the user input the feedback
!from the wordle program, and doing this over and over until the user wants
!to stop.  technically, you can go more than 6 tries.

!initial values of variables
IS_MUST(1)=0
ML(1)=' '
IS_MUST(2)=0
ML(2)=' '
IS_MUST(3)=0
ML(3)=' '
IS_MUST(4)=0
ML(4)=' '
IS_MUST(5)=0
ML(5)=' '

!counting the number of letters that "must be somewhere but not in the place where it was found"
count_MBS = 0
!counting the number of letters that must not be in the word.
count_NOT_ANYWHERE = 0

do l=1,5
	count_NOT_HERE(l) = 0
enddo

try_count=0

15 continue

try_count = try_count + 1



write(*,*) ' '
write(*,*) ' Will now have you enter the feedback for Try number ', try_count

do l=1,5

write(*,*) ' For Letter Position ',l,':'
write(*,*) '     Enter  1, if the wordle feedback was a MUST for this letter in this particular position'
write(*,*) '     Enter  0, if the wordle feedback was a MUST for this letter but NOT in this particular position'
write(*,*) '     Enter -1, if the wordle feedback was a NOT anywhere in the word for this letter (or not in this'
write(*,*) '               position if it already appears in some other position'
	read(*,*) idoc

	if(idoc.eq.1) then
  		IS_MUST(l)=1
  		write(*,*) ' Enter the MUST letter for Position ',l, ' (use capital letter please):'
  		read(*,*)  ML(l)
	elseif(idoc.eq.0) then
		count_MBS = count_MBS + 1

		write(*,*) ' Enter the MUST-BE-SOMEWHERE-BUT-NOT-HERE Letter (use capital letter please):'
                read(*,*) MBS(count_MBS)

		count_NOT_HERE(l) = count_NOT_HERE(l) + 1
                NOT_HERE(l,count_NOT_HERE(l)) = MBS(count_MBS)
	else
  		count_NOT_ANYWHERE = count_NOT_ANYWHERE + 1 
  		write(*,*) ' Enter the NOT-ANYWHERE letter for the wordle word (use capital letter please):'
  		read(*,*) NOT_ANYWHERE(count_NOT_ANYWHERE)
!If a letter is supposed to appear only once and you put it in twice, it will be keyed as -1 for one of them
!and we don't want that, though it does mean that it can't be in this new position.
!So, I need to check if a NOT_ANYWHERE letter is already an MBS or MUST letter.  If it is, then don't mark it as NOT_ANYWHERE,
!but do add to the NOT_HERE list for this position.

		!check to see if this NOT_ANYWHERE letter is already an MBS letter or a MUST letter
		if(count_MBS.eq.0) go to 135 !skip this check if there are no MBS letters
		do k=1,count_MBS
			if(NOT_ANYWHERE(count_NOT_ANYWHERE).eq.MBS(k)) then
				!if we get to here we have a NOT_ANYWHERE letter that is also an MBS letter
				!so, add another location where this letter cannot appear
				count_NOT_HERE(l) = count_NOT_HERE(l) + 1
				NOT_HERE(l,count_NOT_HERE(l)) = NOT_ANYWHERE(count_NOT_ANYWHERE)
				!and subtract one from the count_NOT_ANYWHERE counter
				count_NOT_ANYWHERE = count_NOT_ANYWHERE - 1
				!pretty sure this can only happen for one MBS letter, so skip out of this now
				go to 145
			endif
		enddo
		135 continue
		!if we get to here, the NOT_ANYWHERE letter did not match an MBS letter (or there are no MBS letters)
		!so, we still need to check if it matches a MUST letter.  If it does, then we are going to 
		!again add it to the NOT_HERE list for this position.
		do k=1,5  !loop through the positions
			if(IS_MUST(k).eq.1) then
				if(NOT_ANYWHERE(count_NOT_ANYWHERE).eq.ML(k)) then
					!if we get here we have a NOT_ANYWHERE letter that is also a MUST letter
					!in some other position
					!so, add another location where this letter cannot appear
					count_NOT_HERE(l) = count_NOT_HERE(l) + 1
					NOT_HERE(l,count_NOT_HERE(l)) = NOT_ANYWHERE(count_NOT_ANYWHERE)
					!and subtract one from the count_NOT_ANYWHERE counter
					count_NOT_ANYWHERE = count_NOT_ANYWHERE - 1
					!I don't think it matters if there is more than one MUST letter that matches
					!because it'll be the same letter and we'd do the same thing:
					!mark it as a NOT_HERE letter.
					go to 145
				endif
			endif
		enddo
		145 continue		
	
	endif

enddo
!At this point we are at the end of the try and can write out all the possible wordle words that are left
!How do we do this?  I guess we simply loop through all the words and print out all the possible words.
!I guess we can print to both the screen and to a file.

do i=1,count_wordle
	!see if it breaks any of the rules.  

	!First let's see it it contains any NOT_ANYWHERE letters
	if(count_NOT_anywhere.eq.0) go to 95
	
	do k=1,count_NOT_anywhere
		do j=1,5
			If(W(i,j).eq.NOT_ANYWHERE(k)) then
				POSSIBLE(i)=0
				!go to the next word in the wordle list
				go to 35
			endif
		enddo
	enddo

	95 continue

	!if we make it this far, the word avoided all the NOT_ANYWHERE letters.
	!Now let's check that it has the MBS letters
	if(count_MBS.eq.0) go to 105

	do k=1,count_MBS
		MBS_chk(k)=0
	enddo

	do k=1,count_MBS
		do j=1,5
			if(W(i,j).eq.MBS(k)) then
				MBS_chk(k)=1
			endif
		enddo
	enddo

	do k=1,count_MBS
		if(MBS_chk(k).eq.0) then
			!the wordle word is missing an MBS letter and thus is not a possible answer
			POSSIBLE(i)=0
			!go to the next word in the wordle list
			go to 35
		endif
	enddo

	!I think this is a good place to make sure an MBS is not used in the wrong place
	do j=1,5
		if(count_NOT_here(j).eq.0) goto 115
		do k=1,count_NOT_here(j)
			if(W(i,j).eq.NOT_HERE(j,k)) Then
				!if the letter in j^th position is equal to a NOT_HERE letter for this position
				!then this wordle word is not a possible answer
				POSSIBLE(i)=0
				!go to the next word in the wordle list
				go to 35
			endif
		enddo
	115 continue
	enddo

	105 continue
!at this point we've avoided the NOT_ANYWHERE letters, and we have all the MBS letters, and 
! we have avoided using an MBS letter in the wrong position
! So, we still need to check the letters that must be used in a particular position

	do j=1,5
		if(IS_MUST(j).eq.1) then
   			if(W(i,j).ne.ML(j)) then
     				POSSIBLE(i)=0
				! go to the next word in the wordle list
     				go to 35
			else
			!if the letter matches the MUST letter for this position, 
			!then go to the next positions to check out any MUSTs there
		     		go to 45
			endif

			!if we made it this far, then we've checked all the MUSTs and 
			!can go to the next word in the wordle list
			go to 35
		endif

	45 continue 
	enddo
35 continue
enddo

!Now we can check if all the words with POSSIBLE=1 to see if any have already been used.

do i=1,count_wordle

	if(POSSIBLE(i).eq.1) then
		do k=1,count_wordle_used
			do j=1,5
				if(W(i,j).eq.USED(k,j)) then
					!go to the next letter
					go to 55
				else
					!go to the next used word
					go to 65
				endif
			55 continue
			!if we make it this far with j=5, then the word has already been used
			if(j.eq.5) then 
				POSSIBLE(i)=0
				!go to the next value of i
				go to 75
			endif
			enddo				
		65 continue
		enddo
	endif
75 continue
enddo

!Now we can print out all the wordle words with POSSIBLE(i)=1

do i=1,count_wordle

	if(POSSIBLE(i).eq.1) then
		write(*,*) (W(i,j), j=1,5) 
!		write(9,*) (W(i,j), j=1,5)
	endif
enddo

write(*,*) ' '
write(*,*) ' Do you want to enter the wordle feedback for another try?'
write(*,*) ' Enter 1 for Yes'
write(*,*) ' Enter 0 for No'
read(*,*) idec
if(idec.eq.1) go to 15

end
