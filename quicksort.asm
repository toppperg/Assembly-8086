        PAGE 60,80
        TITLE quick_sort
        
STACKSG SEGMENT PARA STACK 'STACK'
        DW 64 DUP(?)
STACKSG ENDS

DATASG  SEGMENT PARA 'DATA'
DIZI    DB 100 dup(0)
ELEMAN  DB 100
MSG2	DB	'Enter the Length of the array','$'
MSG_neg	DB	'-','$'
MSG_val	DB	' Enter the number  ','$'
MSG_err DB	'You are allowed to number from -128 to 127 only ', '$'
MSG4 	DB	'-- UNSORTED ARRAY --', '$'
MSG5 	DB	'-- SORTED ARRAY --', '$'
MSG_voi	DB	'  ','$'
the_string db 26         ;MAXIMUM CHARACTERS: 25
           db ?          ;ENTERED CHARACTER LENGTH
           db 26 dup (?) ;ENTERED CHARACTERS
DATASG ENDS

CODESG  SEGMENT PARA 'CODE'
        ASSUME CS:CODESG, DS:DATASG, SS:STACKSG

BASLA   PROC FAR
        PUSH DS
        XOR AX,AX
        PUSH AX
        MOV AX, DATASG
        MOV DS, AX
		;----------;
		
		LEA DX,MSG2	 ; came second
		MOV AH,9
		INT 21H
		
		CALL read_number ; Request the length of the string and pass it to BX
		CALL new_line
		PUSH BX ; Put the number of elements on the stack
		
		XOR CX,CX
		MOV CL,BL ;  Prepare the number of loops, it will rotate as many as the number of elements.
		
		XOR SI,SI ; LOOP THAT RECEIVES ELEMENTS FROM THE USER
getElement:
		MOV BX,SI
		INC BL
		CALL print	; Prints BL+1 value to the screen

		LEA DX,MSG_val	; ". Print the string ".element:"
		MOV AH,9
		
		INT 21H

		CALL read_number ; read it and put it on BX
		
		CMP BX,128
		JGE	error1    ; Error if greater than or equal to 128
		CMP BX,-128
		JL error1	;  Error if less than -128
		JMP noerror
		error1:
			LEA DX,MSG_err ; ERROR MESSAGE
			MOV AH,9
			INT 21H
			DEC SI ; Let's decrease it here so that the SI value does not increase by one.
			INC CX ; Let's increase it here so that the loop does not go forward.
			JMP errorcontinue
		noerror:
		
		MOV dizi[SI],BL ;add the entered value to the array.
		errorcontinue:
		CALL new_line
		INC SI
		LOOP getElement
		
		CALL new_line	; new line
		LEA DX,MSG4	; Print the string "original array"
		MOV AH,9
		INT 21H
		CALL new_line	; new line
		
		POP CX
		PUSH CX ; Put it back on the stack, you'll need n numbers.
		XOR SI,SI
showArray:
		XOR BH,BH
		MOV BL, dizi[SI]
		CALL print	; Prints the BL value to the screen
		
		LEA DX,MSG_voi	; print space.
		MOV AH,9
		INT 21H
		
		INC SI
		LOOP showArray
		;;;;;;;;;;;;;;;;;;;  START QUICKSORT ;;;;;;;;;;;;;;;;;;;;;;
		CALL new_line
		CALL new_line
		LEA DX,MSG5	; Print the string "sorted array".
		MOV AH,9
		INT 21H
		
		; Quicksort will take two parameters; LEFT:AX, RIGHT:BX
		XOR AX,AX ; left = 0
		POP BX ; right = BX = n
		PUSH BX ; will be used below (n)
		DEC BX ; right = n-1
		CALL  quicksort
		
		CALL new_line
		
		;Print sorted array
		POP CX  ; cx = n
		XOR SI,SI
showArray2:
		XOR BH,BH
		MOV BL, dizi[SI]
		CALL print	; Prints the BL value to the screen
		
		LEA DX,MSG_voi	; print space.
		MOV AH,9
		INT 21H
		
		INC SI
		LOOP showArray2
		
        RETF
BASLA   ENDP

quicksort	PROC
	; LEFT:AX, RIGHT:BX
PUSH BX
PUSH AX
	CMP AX,BX
	JGE bitti
	CALL arrange_pivot ; Freezes the new location of the pivot via DX
	
POP AX
	MOV BX,DX ; transfer pivot
	DEC BX ; right = pivot - 1
	CALL quicksort ; (left:left, right:pivot-1)
POP BX
	MOV AX,DX ; transfer pivot
	INC AX ; left = pivot + 1
	CALL quicksort ; (left:pivot+1, right:right)
	JMP contiunee
	bitti:
	POP AX
	POP BX
	contiunee:
	RET
quicksort	ENDP

arrange_pivot PROC
	XOR DH,DH
	; DX: pivot, AX: left, BX: right
	MOV DL, dizi[BX] ; DX = dizi[right]
	MOV SI,AX
	DEC SI ; ind = SI = left - 1
	
	MOV CX,BX
	SUB CX,AX ; Our number of loops: right-left
	
	MOV DI,AX ; j = left
loop1:
	PUSH BX
	XOR BH,BH
	MOV BL, dizi[DI]
	CMP BL,DL
	JG continue2
	INC SI
	
	;;; swap array[SI] with array[DI]
	PUSH DX
	XOR DH,DH
	MOV DL,dizi[SI]
	XCHG DL,dizi[DI]
	MOV dizi[SI],DL
	POP DX
	
	continue2:
	POP BX
	INC DI
	LOOP loop1

	 ;;; swap array[SI+1] with array[BX]
	PUSH DX
	XOR DH,DH
	INC SI ; SI+1
	MOV DL,dizi[SI]
	XCHG DL,dizi[BX]
	MOV dizi[SI],DL
	POP DX
	
	MOV DX,SI ;; DX = SI+1 döndür

	RET
arrange_pivot ENDP


read_number	PROC
PUSH SI
	; It asks the user for a number. Assigns to BX value

	MOV AH, 0ah  ; karakter oku
	mov dx,offset the_string  ; burada tut
	INT 21H
	
	MOV SI, offset the_string ;convert string to number (export to SI)
	CALL string2num	;throw the number into BX
POP SI
	RET
read_number ENDP


new_line	PROC
		PUSH DX
		PUSH AX
		
		MOV dl, 10	; NEW LINE \n
		MOV ah, 02h
		INT 21h
		MOV dl, 13
		MOV ah, 02h
		INT 21h
		
		POP AX
		POP DX
		RET
new_line ENDP

print		PROC
push ax
push dx
push cx
push bx
	mov  ax, bx ; BL value is used
	CMP bl,0
	JGE continueee ; If negative
		LEA DX,MSG_neg ; - print symbol
		MOV AH,9
		INT 21H
		MOV AX,BX ; throw the value to be suppressed back to ax
		NEG Al ; Multiply by -1, make it look positive	
	continueee:
	
   MOV BX, 10     ;We will use bx as divisor
   XOR DX,DX
   XOR CX,CX
    
          ;division operations
bol1:  
	XOR DX,DX
    DIV BX      ;AX/BX
    PUSH DX     
    INC CX      ;number of loops to follow the numbers
    CMP AX, 0     ;Is there anything left to share?
    JNE bol1     ; couting finished
    
bol2:  POP DX      
   ADD DX, 30H     ;convert to ascii equivalent
   MOV AH, 02H     
   INT 21H      ;show character
   LOOP bol2    

pop bx
pop cx
pop dx
pop ax
  RET
print ENDP

string2num	PROC
push cx ; We do not want its value to change in the main procedure.
push si
;Let's make SI the address of the lowest significant digit
  inc  si ;It was taken to SI, the address of the number of characters entered
  mov  cl, [ si ] ;number of characters entered                                     
  xor ch,ch ;was reset
  add  si, cx ; SI: indicates the least significant digit
;string transformation
  xor bx,bx
  mov  bp, 1 ;We will multiply it by 10 to reach its real value MULTIPLE OF 10 TO MULTIPLY EVERY DIGIT.

  xor ax,ax
repeat:
;convert characters                    
  mov  al, [ si ] ;transfer the figure to al
  CMP AL,'-'
  JNE positive
		; If the number is negative (if we are in the '-' character)
		NEG BX
		JMP negative  ; döngüyü bitir
  positive:
  sub  al, 48 ;convert ascii character to number

  xor ah,ah
  mul  bp ;AX*BP = DX:AX
  add  bx,ax ;send the result to bx
;Increase the multiples of 10 as 1, 10, 100..
  mov  ax, bp
  mov  bp, 10
  mul  bp ;AX*10 = DX:AX
  mov  bp, ax ;a new multiple of 10
;Check if finished
  dec  si ;next digit to be processed
  loop repeat
negative:

pop si
pop cx
  RET
string2num ENDP

CODESG  ENDS
        END BASLA

