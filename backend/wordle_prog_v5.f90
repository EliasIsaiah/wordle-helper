!October 27, 2022. Modified the program to provide the user with an option
! to evaluate the list of possible words by showing what the wordle output 
! would be if you give the program a word and it treats each word in the list
! as the correct word. That way you can see if a given word can differentiate among
! all the possible words as a throw-away word.  I might restrict this option to
! only "short" lists, but I'm not sure how short "short" should be.  Logic would 
! say that the list should be no longer than 32 because 2^5 is 32.  Practically, 
! I would guess that 20 might be a good cut-off. But, then again, there are often
! words in the list that I think are not likely.  That is why a separate program 
! to do this is also something that I might do.  So, I think I'll restrict it to
! 20, but also have a separate program where a person can input words.
! 
!October 24, 2022. Modified the program to print out the letter distribution
! among the remaining words.  Prints out how many words have an A, 
! how many have a B, how many have a C, etc.
!
!October 22, 2022. Modified the program to provide the user the option of 
! updating "wordle_list_used.txt" by supplying the winning word.
!
!October 8, 2022.  Program to help me save time looking up words for Wordle.
! I tell the program what feedback I've had from Wordle, and it tells me what
! possible words are left.  That's all.
!*********************************************************************************

PROGRAM wordle_help
IMPLICIT NONE

INTEGER position_check_1(50), position_check_2(50)
INTEGER first_time

INTEGER kount_words_with_A, kount_words_with_B, kount_words_with_C, kount_words_with_D
INTEGER kount_words_with_E, kount_words_with_F, kount_words_with_G, kount_words_with_H
INTEGER kount_words_with_I, kount_words_with_J, kount_words_with_K, kount_words_with_L
INTEGER kount_words_with_M
INTEGER kount_words_with_N, kount_words_with_O, kount_words_with_P, kount_words_with_Q
INTEGER kount_words_with_R, kount_words_with_S, kount_words_with_T, kount_words_with_U
INTEGER kount_words_with_V, kount_words_with_W, kount_words_with_X, kount_words_with_Y
INTEGER kount_words_with_Z
INTEGER chk_A,chk_B,chk_C,chk_D,chk_E,chk_F,chk_G,chk_H,chk_I,chk_J,chk_K,chk_L,chk_M
INTEGER chk_N,chk_O,chk_P,chk_Q,chk_R,chk_S,chk_T,chk_U,chk_V,chk_W,chk_X,chk_Y,chk_Z

INTEGER i, j, k, l, count_wordle, count_wordle_used, try_count, idoc, idec
INTEGER count_NOT_HERE(5), count_NOT_ANYWHERE

INTEGER MBS_chk(100), count_MBS
CHARACTER*1 cjunk, MBS(10), NOT_HERE(5,100), NOT_ANYWHERE(100)

CHARACTER*1 W(5000,5), proposed_word(5), output(5)
! W(5000,5) are the 2309 Wordle words, one character for each of the five letters.
! W(j,i)= the value of the letter in the i^th position in the j^th word.

CHARACTER*1 USED(5000,5)
! USED(5000,5) are the Wordle words that have been used so far in the game.

CHARACTER*1 winning_word(5)
! winning_word(5) is the winning Wordle word for the day.

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

i=0
do 
	i=i+1
	read(1,11,end=5) (W(i,j), j=1,5) 
        POSSIBLE(i)=1
! at first, all wordle words are possible to be the answer
11	format(5a1)

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
count_wordle_used = i-1
write(*,*) 'number of used wordle words read in = ',count_wordle_used

CLOSE(1)

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

!initializing the counter for the number of letters not allowed in a particular position
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
  		write(*,*) ' Enter the MUST letter for Position ',l, ':'
  		read(*,*)  cjunk
		call cap_check(cjunk)
		ML(l)=cjunk
	elseif(idoc.eq.0) then
		count_MBS = count_MBS + 1
		write(*,*) ' Enter the MUST-BE-SOMEWHERE-BUT-NOT-HERE Letter:'
                read(*,*) cjunk
		call cap_check(cjunk)
		MBS(count_MBS)=cjunk

		count_NOT_HERE(l) = count_NOT_HERE(l) + 1
                NOT_HERE(l,count_NOT_HERE(l)) = MBS(count_MBS)
	else
  		count_NOT_ANYWHERE = count_NOT_ANYWHERE + 1 
  		write(*,*) ' Enter the NOT-ANYWHERE letter for the wordle word:'
  		read(*,*) cjunk
		call cap_check(cjunk)
		NOT_ANYWHERE(count_NOT_ANYWHERE)=cjunk
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
kount_words_with_A = 0
kount_words_with_B = 0
kount_words_with_C = 0
kount_words_with_D = 0
kount_words_with_E = 0
kount_words_with_F = 0
kount_words_with_G = 0
kount_words_with_H = 0
kount_words_with_I = 0
kount_words_with_J = 0
kount_words_with_K = 0
kount_words_with_L = 0
kount_words_with_M = 0
kount_words_with_N = 0
kount_words_with_O = 0
kount_words_with_P = 0
kount_words_with_Q = 0
kount_words_with_R = 0
kount_words_with_S = 0
kount_words_with_T = 0
kount_words_with_U = 0
kount_words_with_V = 0
kount_words_with_W = 0
kount_words_with_X = 0
kount_words_with_Y = 0
kount_words_with_Z = 0


do i=1,count_wordle
chk_A = 0
chk_B = 0
chk_C = 0
chk_D = 0
chk_E = 0
chk_F = 0
chk_G = 0
chk_H = 0
chk_I = 0
chk_J = 0
chk_K = 0
chk_L = 0
chk_M = 0
chk_N = 0
chk_O = 0
chk_P = 0
chk_Q = 0
chk_R = 0 
chk_S = 0
chk_T = 0
chk_U = 0
chk_V = 0
chk_W = 0
chk_X = 0
chk_Y = 0
chk_Z = 0
	if(POSSIBLE(i).eq.1) then
		write(*,*) (W(i,j), j=1,5) 
		do j=1,5
			if(W(i,j).eq.'A') then
				if(chk_A.eq.0) then
					kount_words_with_A = kount_words_with_A + 1
					chk_A = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'B') then
				if(chk_B.eq.0) then
					kount_words_with_B = kount_words_with_B + 1
					chk_B = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'C') then
				if(chk_C.eq.0) then
					kount_words_with_C = kount_words_with_C + 1
					chk_C = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'D') then
				if(chk_D.eq.0) then
					kount_words_with_D = kount_words_with_D + 1
					chk_D = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'E') then
				if(chk_E.eq.0) then
					kount_words_with_E = kount_words_with_E + 1
					chk_E = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'F') then
				if(chk_F.eq.0) then
					kount_words_with_F = kount_words_with_F + 1
					chk_F = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'G') then
				if(chk_G.eq.0) then
					kount_words_with_G = kount_words_with_G + 1
					chk_G = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'H') then
				if(chk_H.eq.0) then
					kount_words_with_H = kount_words_with_H + 1
					chk_H = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'I') then
				if(chk_I.eq.0) then
					kount_words_with_I = kount_words_with_I + 1
					chk_I = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'J') then
				if(chk_J.eq.0) then
					kount_words_with_J = kount_words_with_J + 1
					chk_J = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'K') then
				if(chk_K.eq.0) then
					kount_words_with_K = kount_words_with_K + 1
					chk_K = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'L') then
				if(chk_L.eq.0) then
					kount_words_with_L = kount_words_with_L + 1
					chk_L = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'M') then
				if(chk_M.eq.0) then
					kount_words_with_M = kount_words_with_M + 1
					chk_M = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'N') then
				if(chk_N.eq.0) then
					kount_words_with_N = kount_words_with_N + 1
					chk_N = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'O') then
				if(chk_O.eq.0) then
					kount_words_with_O = kount_words_with_O + 1
					chk_O = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'P') then
				if(chk_P.eq.0) then
					kount_words_with_P = kount_words_with_P + 1
					chk_P = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'Q') then
				if(chk_Q.eq.0) then
					kount_words_with_Q = kount_words_with_Q + 1
					chk_Q = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'R') then
				if(chk_R.eq.0) then
					kount_words_with_R = kount_words_with_R + 1
					chk_R = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'S') then
				if(chk_S.eq.0) then
					kount_words_with_S = kount_words_with_S + 1
					chk_S = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'T') then
				if(chk_T.eq.0) then
					kount_words_with_T = kount_words_with_T + 1
					chk_T = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'U') then
				if(chk_U.eq.0) then
					kount_words_with_U = kount_words_with_U + 1
					chk_U = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'V') then
				if(chk_V.eq.0) then
					kount_words_with_V = kount_words_with_V + 1
					chk_V = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'W') then
				if(chk_W.eq.0) then
					kount_words_with_W = kount_words_with_W + 1
					chk_W = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'X') then
				if(chk_X.eq.0) then
					kount_words_with_X = kount_words_with_X + 1
					chk_X = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'Y') then
				if(chk_Y.eq.0) then
					kount_words_with_Y = kount_words_with_Y + 1
					chk_Y = 1
					go to 125
				else
					go to 125
				endif
			elseif(W(i,j).eq.'Z') then
				if(chk_Z.eq.0) then
					kount_words_with_Z = kount_words_with_Z + 1
					chk_Z = 1
					go to 125
				else
					go to 125
				endif
			endif
			125 continue
		enddo

!		write(9,*) (W(i,j), j=1,5)
	endif
enddo

write(*,*) ' '
write(*,*) ' Do you want the program to provide a list of how many words have each alphabet letter?' 
write(*,*) ' (if a letter does not appear in the list, that means there are zero words with that letter)'
write(*,*) ' Enter 1 for Yes'
write(*,*) ' Enter 0 for No'
read(*,*) idec
if(idec.eq.1) then
	if(kount_words_with_A.ne.0) write(*,*) 'number of words with A = ',kount_words_with_A
	if(kount_words_with_B.ne.0) write(*,*) 'number of words with B = ',kount_words_with_B
	if(kount_words_with_C.ne.0) write(*,*) 'number of words with C = ',kount_words_with_C
	if(kount_words_with_D.ne.0) write(*,*) 'number of words with D = ',kount_words_with_D
	if(kount_words_with_E.ne.0) write(*,*) 'number of words with E = ',kount_words_with_E
	if(kount_words_with_F.ne.0) write(*,*) 'number of words with F = ',kount_words_with_F
	if(kount_words_with_G.ne.0) write(*,*) 'number of words with G = ',kount_words_with_G
	if(kount_words_with_H.ne.0) write(*,*) 'number of words with H = ',kount_words_with_H
	if(kount_words_with_I.ne.0) write(*,*) 'number of words with I = ',kount_words_with_I
	if(kount_words_with_J.ne.0) write(*,*) 'number of words with J = ',kount_words_with_J
	if(kount_words_with_K.ne.0) write(*,*) 'number of words with K = ',kount_words_with_K
	if(kount_words_with_L.ne.0) write(*,*) 'number of words with L = ',kount_words_with_L
	if(kount_words_with_M.ne.0) write(*,*) 'number of words with M = ',kount_words_with_M
	if(kount_words_with_N.ne.0) write(*,*) 'number of words with N = ',kount_words_with_N
	if(kount_words_with_O.ne.0) write(*,*) 'number of words with O = ',kount_words_with_O
	if(kount_words_with_P.ne.0) write(*,*) 'number of words with P = ',kount_words_with_P
	if(kount_words_with_Q.ne.0) write(*,*) 'number of words with Q = ',kount_words_with_Q
	if(kount_words_with_R.ne.0) write(*,*) 'number of words with R = ',kount_words_with_R
	if(kount_words_with_S.ne.0) write(*,*) 'number of words with S = ',kount_words_with_S
	if(kount_words_with_T.ne.0) write(*,*) 'number of words with T = ',kount_words_with_T
	if(kount_words_with_U.ne.0) write(*,*) 'number of words with U = ',kount_words_with_U
	if(kount_words_with_V.ne.0) write(*,*) 'number of words with V = ',kount_words_with_V
	if(kount_words_with_W.ne.0) write(*,*) 'number of words with W = ',kount_words_with_W
	if(kount_words_with_X.ne.0) write(*,*) 'number of words with X = ',kount_words_with_X
	if(kount_words_with_Y.ne.0) write(*,*) 'number of words with Y = ',kount_words_with_Y
	if(kount_words_with_Z.ne.0) write(*,*) 'number of words with Z = ',kount_words_with_Z
endif

write(*,*) ' '
write(*,*) ' Would you like to have the program evaluate a proposed word against all the possible words '
write(*,*) ' to see how each possible word would appear if each possible word was the correct word? ' 
write(*,*) ' '
write(*,*) ' This information can help you determine a good throw-away word, that is, a word for which '
write(*,*) ' wordle output is different for each of the possible wordle words. '
write(*,*) ' '
write(*,*) ' Enter 1 for Yes, you want to propose a word to evaluate all the possible words, '
write(*,*) ' Enter 0 for No'
read(*,*) idec
if(idec.eq.1) then
165 continue
!start the loop on evaluating user-proposed words
	write(*,*) ' Starting in column 1, enter the word you want to evaluate against all the possible words'
	write(*,*) ' (capitalization is not important)'
	read(*,21) (proposed_word(k), k=1,5)

	first_time=0

	do i=1,count_wordle
		if(POSSIBLE(i).eq.1) then
			first_time=first_time + 1
!this is the tough part. figuring out the wordle output
			do k=1,5
				position_check_1(k)=0
				position_check_2(k)=0
				output(k) = '*'
			enddo

			do j=1,5
				call cap_check(proposed_word(j))
			enddo

!first check to see if any letters are correct and in the right place

			do j=1,5
				if(proposed_word(j).eq.W(i,j)) then
!right letter and in the right place
					call cap_check(proposed_word(j))
					output(j) = proposed_word(j)
!keep track of which positions in W(i,j) have been matched to a position in proposed_word()
					position_check_1(j) = 1
				endif
			enddo

			do k=1,5
				if(position_check_1(k).eq.1) go to 175
				do j=1,5
!if jth position in W(i,j) already perfectly matches the same position in proposed_word(), skip this value of j
					if(position_check_1(j).eq.1) go to 155
!if jth position in W(i,j) has already been matched in Loop 2, skip this value of j
					if(position_check_2(j).eq.1) go to 155
					if(proposed_word(k).eq.W(i,j)) then
!right letter wrong place
						call lower_case_check(proposed_word(k))
						output(k) = proposed_word(k)
						position_check_2(j) = 1
						go to 175		
					endif
					155 continue
				enddo
				175 continue
			enddo
!now print out the evaluation for POSSIBLE(i)
if(first_time.eq.1) then
write(*,*) 'For the wordle output below, '
write(*,*) '  an asterisk (*) corresponds to a letter with no match' 
write(*,*) '  a lower case letter corresponds to a matching letter that is in the wrong position'
write(*,*) '  and a capital letter corresponds to a matching letter that is in the right position'
write(*,*) ' '
endif
			write(*,31) (W(i,j), j=1,5), (output(j), j=1,5)
31 format(' When the wordle word = possible word ',5a1,', the wordle output for the proposed word = ',5a1)
		endif
	enddo
write(*,*) ' Do you want to propose another word to use to evaluate all the possible words?'
write(*,*) ' Enter 1 for Yes'
write(*,*) ' Enter 0 for No'
read(*,*) idec
if(idec.eq.1) go to 165
endif


write(*,*) ' '
write(*,*) ' Do you want to enter the wordle feedback for another try?'
write(*,*) ' Enter 1 for Yes'
write(*,*) ' Enter 0 for No'
read(*,*) idec
if(idec.eq.1) go to 15

write(*,*) ' ' 
write(*,*) ' Do you want to update the list of the used wordle words with the winning word?'
write(*,*) ' Enter 1 for Yes'
write(*,*) ' Enter 0 for No'
read(*,*) idec

if(idec.eq.1) then
	write(*,*) ' '
	write(*,*) ' Enter the winning wordle word (starting in column 1 on your screen):'
	read(*,21) (winning_word(j), j=1,5)
21      format(5a1)
	do j=1,5
		call cap_check(winning_word(j))
	enddo

	open(unit=1,file='wordle_list_used.txt')
	do k=1,count_wordle_used
		write(1,21) (USED(k,j), j=1,5)
	enddo
	write(1,21) (winning_word(j), j=1,5)
	CLOSE(1)
endif
end

subroutine cap_check(cjunk)
character*1 cjunk
		if(cjunk.eq.'a') cjunk='A'
		if(cjunk.eq.'b') cjunk='B'
		if(cjunk.eq.'c') cjunk='C'
		if(cjunk.eq.'d') cjunk='D'
		if(cjunk.eq.'e') cjunk='E'
		if(cjunk.eq.'f') cjunk='F'
		if(cjunk.eq.'g') cjunk='G'
		if(cjunk.eq.'h') cjunk='H'
		if(cjunk.eq.'i') cjunk='I'
		if(cjunk.eq.'j') cjunk='J'
		if(cjunk.eq.'k') cjunk='K'
		if(cjunk.eq.'l') cjunk='L'
		if(cjunk.eq.'m') cjunk='M'
		if(cjunk.eq.'n') cjunk='N'
		if(cjunk.eq.'o') cjunk='O'
		if(cjunk.eq.'p') cjunk='P'
		if(cjunk.eq.'q') cjunk='Q'
		if(cjunk.eq.'r') cjunk='R'
		if(cjunk.eq.'s') cjunk='S'
		if(cjunk.eq.'t') cjunk='T'
		if(cjunk.eq.'u') cjunk='U'
		if(cjunk.eq.'v') cjunk='V'
		if(cjunk.eq.'w') cjunk='W'
		if(cjunk.eq.'x') cjunk='X'
		if(cjunk.eq.'y') cjunk='Y'
		if(cjunk.eq.'z') cjunk='Z'
return
end
subroutine lower_case_check(cjunk)
character*1 cjunk
		if(cjunk.eq.'A') cjunk='a'
		if(cjunk.eq.'B') cjunk='b'
		if(cjunk.eq.'C') cjunk='c'
		if(cjunk.eq.'D') cjunk='d'
		if(cjunk.eq.'E') cjunk='e'
		if(cjunk.eq.'F') cjunk='f'
		if(cjunk.eq.'G') cjunk='g'
		if(cjunk.eq.'H') cjunk='h'
		if(cjunk.eq.'I') cjunk='i'
		if(cjunk.eq.'J') cjunk='j'
		if(cjunk.eq.'K') cjunk='k'
		if(cjunk.eq.'L') cjunk='l'
		if(cjunk.eq.'M') cjunk='m'
		if(cjunk.eq.'N') cjunk='n'
		if(cjunk.eq.'O') cjunk='o'
		if(cjunk.eq.'P') cjunk='p'
		if(cjunk.eq.'Q') cjunk='q'
		if(cjunk.eq.'R') cjunk='r'
		if(cjunk.eq.'S') cjunk='s'
		if(cjunk.eq.'T') cjunk='t'
		if(cjunk.eq.'U') cjunk='u'
		if(cjunk.eq.'V') cjunk='v'
		if(cjunk.eq.'W') cjunk='w'
		if(cjunk.eq.'X') cjunk='x'
		if(cjunk.eq.'Y') cjunk='y'
		if(cjunk.eq.'Z') cjunk='z'
return
end
