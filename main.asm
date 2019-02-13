INCLUDE Irvine32.inc
INCLUDE macros.inc
.data				
;###########################MESSAGE BOX#################################

	caption1 byte "Error",0
	caption2 byte "Success",0
	errorname byte "Name not found",0
	errorCCN byte "Credit card number not found",0
	errorfile byte "Can't open file",0
	invalidhandel byte "Invald file handle",0
	errorread byte "Failed to read",0
	errorsize byte"Size of string credit info is not enough",0
	invalidCCN byte "Invalid credit card no",0
	invaildCCV byte "Invalid CVV",0
	errorpay byte "There is no sufficent money", 0
	existName byte "Name already exists",0
	existCCN byte "Credit card no. already exists",0
	infomsg0 byte "Successful payment",0
	infomsg1 byte "Record Added Successfully",0
	infomsg2 byte "Successful Top Up",0
	invalidChoiceMsg byte "Invalid choice" , 0
	validinfomsg byte "Added in valid transactions log file",0
	Invalidinfomsg byte "Added in Invalid transactions log file",0

;########################MESSAGE BOX END################################

;#########################Validation DATA###################################	
	CCStrno BYTE 16 DUP(0), 0
	CCIntno DWORD 16 DUP(0), 0
	CCSize DWORD 0
	CCRemainder DWORD 1 ;initially not valid
;#########################Validation DATA End ##############################	

;#########################CCInfo DATA###################################	
	expiryDate BYTE 7 Dup(0), 0
	expiryDateSize DWORD 0
	CVV BYTE 3 dup(0), 0
	cvvSize dword 0
	holderName BYTE 20 dup(0), 0
	holderNameSize DWORD 0
	balance BYTE 10 DUP(0), 0
	balanceSize DWORD 0
	userRecord BYTE 60 DUP(0), 0dh , 0ah

;#########################CCInfo DATA End ##############################													
														
;#########################PayBy DATA###################################	
	paymentAmount DWORD 0
	balanceToEditSTR BYTE 10 DUP(0),0
	balanceToEditARR DWORD 10 DUP(0)
	balanceToEditInt DWORD 0
	digitsCounter DWORD 0
	tenMultiple DWORD 10

;#########################PayBy DATA End ##############################

;#########################BONUS DATA###################################	
	invaliduserRecord BYTE 80 DUP(0) ,0dh , 0ah

	validuserRecord BYTE 75 DUP(0) ,0dh , 0ah
    topupAmountstr DWORD 0
    paymentAmountstr DWORD 0
    pointer DWORD 0
	paystr BYTE " Successful payment by amount: ",0
	topupstr BYTE " Successful top up by amount: ",0
	validpaymentFileName byte "Valid_Transaction_Log_File.txt" , 0
	validfileHandle handle 0

	valid_credit_Card_Info byte 5000 dup(?)
	validcridetInfoLength dword lengthof valid_credit_Card_Info
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	invpaystr BYTE "Inavlid payment due to: ",0
	invtopupstr BYTE "Inavlid Top Up due to: ",0
	failerreson byte 0
	invalidpaymentFileName byte "InValid_Transaction_Log_File.txt" , 0
	invalidfileHandle handle 0
	invalid_credit_Card_Info byte 5000 dup(?)
	invalidcridetInfoLength dword lengthof invalid_credit_Card_Info
;#########################BONUS DATA End ##############################	

;------------------READ WRITE FILE FN-----------------------------
	NumOfBytesRead dword ?
	numOfBytesWritten dword ?
	paymentFileName byte "Credit_Card_information.txt" , 0
	fileHandle handle ?
;----------------READ WRITE FILE FN END---------------------------
;-------------------Add Record To File----------------------------
;-------------------Function Search By Name In File----------------------------
	foundNameBool dword 1
;-------------------Function Search By Name In File END--------------------------
;-------------------Function Search By Nymber In File----------------------------
	foundNumberBool dword 1
;-------------------Function Search By Nymber In File END------------------------

;----------------Add Record To File END---------------------------
;--------------Read File In CreditCARD Info-----------------------
	credit_Card_Info byte 5000 dup(?)
	cridetInfoLength dword lengthof credit_Card_Info
;-------------Read File In Credit CARD Info END-------------------
;---------------------Top Up Balance------------------------------
	choice byte 5 dup(?)
	nameChoice byte "name" , 0
	numberChoice byte "number" , 0
	numberEntered byte 16 dup(?) 
	countOfNameEntered dword ?
	countOfNumberEntered dword ?
	newAmount dword ?
	amountInFile dword 10 dup(?)
	amountArray byte 10 dup(?)
	amountCount dword 0
	amountInFileInt dword 0
;--------------------Top Up Balance END---------------------------
;--------------------------DELETE---------------------------------
	credit_Card_Info_new byte 500 dup(?)
;------------------------DELETE END-------------------------------

.code


;#################################bonus task#################################
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;in valid;;;;;;;;;;;;;;;;;;;;


;--------------------------------------------------------
;Receives: fileHandle
;puts the cursor at the end of the file to append
;--------------------------------------------------------
invMoveCursorToEnd proc
	INVOKE setFilePointer,
	invalidfileHandle,0,0,
	FILE_END
ret
invMoveCursorToEnd ENDP

;------------------------------------------------------
; Opens a new text file and opens for input.
; Receives: variable paymentFileName has the "Credit_Card_information.txt" file
; Returns: If the file was opened successfully, EAX
; contains a valid file handle. Otherwise, EAX equals
; INVALID_HANDLE_VALUE.
;------------------------------------------------------
invalidOpenPaymentFileToWrite proc
	INVOKE CreateFile,
	ADDR invalidpaymentFileName,
	GENERIC_WRITE,
	DO_NOT_SHARE,
	NULL,
	OPEN_Existing,
	FILE_ATTRIBUTE_NORMAL,0
ret
invalidOpenPaymentFileToWrite ENDP
;--------------------------------------------------------
;use variable fileHandle that defined in .data as handle
;use function OpenPaymentFileToWrite , MoveCursorToEnd , WriteToPaymentFile , ClosPaymentFile
;use variable userRecord -> string that will be written in the file
;--------------------------------------------------------
AddinvalidRecordToFile proc uses eax edx ecx 
comment #; ma3mltsh 7aga :'D
;re initialize 
		MOV EDI, OFFSET validuserRecord
		MOV AL, ' '
		MOV ECX, lengthof validuserrecord
		reinit:
		MOV [EDI], AL
        INC EDI
		LOOP reinit
		#
	; write line line in the file
	call invalidOpenPaymentFileToWrite

	;check if file not opened successfully
	cmp eax, INVALID_HANDLE_VALUE 
	jne file_opened 
	;mwrite<"ERROR While Opening", 0dh ,0ah >
	INVOKE MessageBox,NULL,ADDR errorfile,ADDR caption1,MB_OK+MB_ICONSTOP
	ret
	file_opened:
		mov invalidfileHandle , eax
		call invMoveCursorToEnd

		mov eax , invalidfileHandle
		mov edx , offset invaliduserRecord
		mov ecx , lengthof invaliduserRecord
		call WriteToPaymentFile

		mov eax , invalidfileHandle
		call ClosPaymentFile
		;mwrite<"Successfully added" ,0dh , 0ah >
		INVOKE MessageBox,NULL,ADDR invalidinfomsg,ADDR caption2,MB_OK+MB_ICONINFORMATION
	ret
AddinvalidRecordToFile ENDP
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end invalid;;;;;;;;;;;;;;;;;;;
;--------------------------------------------------------
;Receives: fileHandle
;puts the cursor at the end of the file to append
;--------------------------------------------------------
vMoveCursorToEnd proc
	INVOKE setFilePointer,
	validfileHandle,0,0,
	FILE_END
ret
vMoveCursorToEnd ENDP

;------------------------------------------------------
; Opens a new text file and opens for input.
; Receives: variable paymentFileName has the "Credit_Card_information.txt" file
; Returns: If the file was opened successfully, EAX
; contains a valid file handle. Otherwise, EAX equals
; INVALID_HANDLE_VALUE.
;------------------------------------------------------
validOpenPaymentFileToWrite proc
	INVOKE CreateFile,
	ADDR validpaymentFileName,
	GENERIC_WRITE,
	DO_NOT_SHARE,
	NULL,
	OPEN_Existing,
	FILE_ATTRIBUTE_NORMAL,0
ret
validOpenPaymentFileToWrite ENDP
;--------------------------------------------------------
;use variable fileHandle that defined in .data as handle
;use function OpenPaymentFileToWrite , MoveCursorToEnd , WriteToPaymentFile , ClosPaymentFile
;use variable userRecord -> string that will be written in the file
;--------------------------------------------------------
AddvalidRecordToFile proc uses eax edx ecx 
comment @ ;ma3mltsh 7aga
;re initialize 
		MOV EDI, OFFSET validuserRecord
		MOV AL, ' '
		MOV ECX, lengthof validuserrecord
		reinit:
		MOV [EDI], AL
        INC EDI
		LOOP reinit
		@
	; write line line in the file
	call validOpenPaymentFileToWrite

	;check if file not opened successfully
	cmp eax, INVALID_HANDLE_VALUE 
	jne file_opened 
	;mwrite<"ERROR While Opening", 0dh ,0ah >
	INVOKE MessageBox,NULL,ADDR errorfile,ADDR caption1,MB_OK+MB_ICONSTOP
	ret
	file_opened:
		mov validfileHandle , eax
		call vMoveCursorToEnd


		MOV ECX, CCsize
		MOV ESI, OFFSET CCStrno
		MOV EDI, OFFSET validuserRecord
		CLD
		rep MOVSB
		INC EDI
		MOV AL, ' '
		MOV [EDI], AL
		ADD EDI, TYPE validuserrecord

		MOV ESI, OFFSET holdername
		MOV ECX, holdernameSize
		CLD
		rep MOVSB

		CMP pointer, 1
		JNE _topup
		;;;;3ayza a2ol ana gaya mn  function pay ele hya b 1
		MOV ECX, lengthof paystr
		MOV ESI, OFFSET paystr
		CLD
		rep MOVSB
		ADD EDI, 1

		MOV EBX, paymentamountstr
		JMP _cont
		
		_topup:
		MOV EBX, topupamountstr
		MOV ECX, lengthof topupstr
		MOV ESI, OFFSET topupstr
		CLD
		rep MOVSB
		ADD EDI, 1
	     
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		_cont:
		MOV EAX, EBX; holds the number at the beginning
	    intToCharLoop:

		;3aiza 2ml mod ll ebx 3shan 25od kol digit w 27welha l char
		MOV EDX, 0;'el remainder' initialize it every time
		MOV EBX, 10; el maqam
		DIV EBX
		OR EDX, 00110000b;take the remainder to convert it to char
		MOV [EDI], DL;3shan el string ely byshawer 3lih el ESI no3o BYTE

		CMP EAX, 0;3shan el rkm b3d ma etra7 3dad el digits momkn tkon 2a2l mn el asly ele fl file
		JE _outt 
		ADD EDI, 1;3shan lama ytl3 mn  el loop yb2a wa2f 3la a5r el rkm

		LOOP intToCharLoop
		_outt: ;m7tageen n3dl el string w nzwed b ba2e el ecx msafat
		
		SUB ECX, 1; 3shan 3mlna break mn el loop 2bl ma el ecx yn2s f a5r lafa
		;SUB digitscounter, ECX;number of digits of the new balance
		MOV EAX, digitsCounter

		; 3shan a5ly el esi wa2ef 3la2wl element
		SUB EDI, digitscounter
		INC EDI
	
		;kda w2ft el edx 3la a5r element
		MOV EDX, EDI
		ADD EDX, digitscounter
		SUB EDX, 1

		;b3dl el string
	    MOV ECX, digitscounter;loop b 3dd digits el balnce el gdeed
		SHR ECX, 1; 3shan ymshy 3la nos el string
		CMP ECX, 0;3shan law howa digit wa7da may3mlsh reOrder 
		JE _dontReOrderStr                                       
		_reorderStr:

			MOV BL, [EDI]
			XCHG BL, [EDX]
			XCHG BL, [EDI]
			INC EDI
			DEC EDX

		LOOP _reorderStr
		;INC EDI
		_dontReOrderStr: 
	    ;MOV BYTE PTR [EDI], 0dh
		;inc edi
		;MOV BYTE PTR [EDI], 0ah
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		mov eax , validfileHandle
		mov edx , offset validuserRecord
		mov ecx , lengthof validuserRecord
		call WriteToPaymentFile

		mov eax , validfileHandle
		call ClosPaymentFile
		;mwrite<"Successfully added" ,0dh , 0ah >
		INVOKE MessageBox,NULL,ADDR validinfomsg,ADDR caption2,MB_OK+MB_ICONINFORMATION
	ret
AddvalidRecordToFile ENDP
	
;#################################END bonus task#################################

;--------------------------------------------------------
; Reads an input file into a buffer.
; Receives: EAX = file handle, EDX = buffer offset,
; ECX = number of bytes to read
; Returns: If CF = 0, EAX = number of bytes read; if
; CF = 1, EAX contains the system error code returned
; by the GetLastError Win32 API function.
;--------------------------------------------------------
ReadFromPaymentFile proc
	INVOKE ReadFile,
	eax,
	edx,
	ecx,
	ADDR NumOfBytesRead,0
	mov eax , NumOfBytesRead
ret
ReadFromPaymentFile ENDP

;------------------------------------------------------
; Opens a new text file and opens for input.
; Receives: variable paymentFileName has the "Credit_Card_information.txt" file
; Returns: If the file was opened successfully, EAX
; contains a valid file handle. Otherwise, EAX equals
; INVALID_HANDLE_VALUE.
;------------------------------------------------------
OpenPaymentFileToWrite proc
	INVOKE CreateFile,
	ADDR paymentFileName,
	GENERIC_WRITE,
	DO_NOT_SHARE,
	NULL,
	OPEN_Existing,
	FILE_ATTRIBUTE_NORMAL,0
ret
OpenPaymentFileToWrite ENDP

OpenPaymentFileToRead proc
	INVOKE CreateFile,
	ADDR paymentFileName,
	GENERIC_READ,
	DO_NOT_SHARE,
	NULL,
	OPEN_Existing,
	FILE_ATTRIBUTE_NORMAL,0
ret
OpenPaymentFileToRead ENDP

OpenAndTruncationPaymentFileToWrite proc
	INVOKE CreateFile,
	ADDR paymentFileName,
	GENERIC_WRITE,
	DO_NOT_SHARE,
	NULL,
	TRUNCATE_EXISTING,
	FILE_ATTRIBUTE_NORMAL,0
ret
OpenAndTruncationPaymentFileToWrite ENDP

;--------------------------------------------------------
;Receives: fileHandle
;puts the cursor at the end of the file to append
;--------------------------------------------------------
MoveCursorToEnd proc
	INVOKE setFilePointer,
	fileHandle,0,0,
	FILE_END
ret
MoveCursorToEnd ENDP

;--------------------------------------------------------
; Receives: EAX = file handle, EDX = offset el string el hyktbo,
; ECX = number of bytes to write
; Returns: EAX = number of bytes written to the file.
; If the value returned in EAX is less than the
; argument passed in ECX, an error likely occurred.
;--------------------------------------------------------
WriteToPaymentFile proc	
	INVOKE WriteFile,
	eax,
	edx,
	ecx,
	ADDR numOfBytesWritten , 0	
	mov eax , numOfBytesWritten
ret
WriteToPaymentFile ENDP

;--------------------------------------------------------
; Receives: EAX = file handle
; Returns: EAX = nonzero if the file is successfully closed.
;--------------------------------------------------------
ClosPaymentFile proc
	INVOKE CloseHandle , eax
ret
ClosPaymentFile ENDP

;--------------------------------------------------------
;use variable fileHandle that defined in .data as handle
;use function OpenPaymentFileToWrite , MoveCursorToEnd , WriteToPaymentFile , ClosPaymentFile
;use variable userRecord -> string that will be written in the file
;--------------------------------------------------------
AddRecordToFile proc uses eax edx ecx 
	; write line line in the file
	call OpenPaymentFileToWrite

	;check if file not opened successfully
	cmp eax, INVALID_HANDLE_VALUE 
	jne file_opened 
	;mwrite<"ERROR While Opening", 0dh ,0ah >
	INVOKE MessageBox,NULL,ADDR errorfile,ADDR caption1,MB_OK+MB_ICONSTOP
	ret
	file_opened:
		mov fileHandle , eax
		call MoveCursorToEnd
		mov eax , fileHandle
		mov edx , offset userRecord
		mov ecx , lengthof userRecord
		call WriteToPaymentFile

		mov eax , fileHandle
		call ClosPaymentFile
		;mwrite<"Successfully added" ,0dh , 0ah >
		INVOKE MessageBox,NULL,ADDR infomsg1,ADDR caption2,MB_OK+MB_ICONINFORMATION
	ret
AddRecordToFile ENDP
	
	
;--------------------------------------------------------------------------------
;Returns: credit_Card_Info contains the whole file
;         cridetInfoLength = num Of Bytes Read
; use variable filehandle
; use functions OpenPaymentFileToRead , ReadFromPaymentFile , ClosPaymentFile
;--------------------------------------------------------------------------------
ReadFileInCreditInfo proc 
pushad
	; open and read the credit card information file
	call OpenPaymentFileToRead

	;check if file not opened successfully
	cmp eax, INVALID_HANDLE_VALUE 
	jne fileToReadOpened 
	;mwrite<"Error , invald file handle", 0dh , 0ah >
	INVOKE MessageBox,NULL,ADDR invalidhandel,ADDR caption1,MB_OK+MB_ICONSTOP
	exit

 fileToReadOpened:
	mov fileHandle , eax
	mov edx , offset credit_Card_Info 
	mov ecx , lengthof credit_Card_Info

	call ReadFromPaymentFile

	;check if cf =1 -> fail to read
	jnc check_Size_Of_credit_Card_Info
	;mwrite<"Error , Fail to read", 0dh , 0ah >
	INVOKE MessageBox,NULL,ADDR errorread,ADDR caption1,MB_OK+MB_ICONSTOP
	exit

 check_Size_Of_credit_Card_Info:
	cmp eax, lengthof credit_Card_Info
	jb sizeIsEnough
	;mwrite<"Error , size of string credit info is not enough", 0dh , 0ah >
	INVOKE MessageBox,NULL,ADDR errorsize,ADDR caption1,MB_OK+MB_ICONSTOP
	exit

 sizeIsEnough:
	;function readfile returned num of bytes read in eax , credit_Card_Info[eax-1] -> last char read ,
	;credit_Card_Info[eax] = 0 to terminate
	mov cridetInfoLength , eax
	mov credit_Card_Info[eax] , 0
	mov eax , fileHandle
	call ClosPaymentFile
	popad
	ret
ReadFileInCreditInfo ENDP

FunctionSearchByName proc
;pushad;mai
	call ReadFileInCreditInfo
	;mov eax, holderNameSize
	;mwrite<"Enter the name" , 0dh , 0ah >
	;mov edx , offset holderName
	;mov countOfNameEntered , eax

	mov edi , offset credit_Card_Info; string ely fih el file kolo
	mov edx , offset holderName
	mov ecx , cridetInfoLength
	mov esi , 0       ;count to get true name
 searchByNameLoop:
	mov al , [edi]
	mov bl , [edx]
	cmp al , bl
	je equallchar
	jmp notequall
	equallchar:
		inc esi 
		inc edx
		inc edi
		jmp next
	notequall:
		inc edi
		cmp esi , 0
		jg newIntialize
		jmp next
	newIntialize:
		mov esi , 0
		mov edx , offset holderName
	next:
		cmp esi , countOfNameEntered
		je _nameFound
	loop searchByNameLoop

	;mwrite<"Name not found" , 0dh , 0ah >
	jmp _notFound
	_nameFound:
	mov foundNameBool , 1
	JMP endFunctionSearchByName
	_notFound:
	mov foundNameBool, 0
	endFunctionSearchByName:
	;popad ;added mai
ret
FunctionSearchByName ENDP

FunctionSearchByNumber proc 
;PUSHAD ;added mai
	call ReadFileInCreditInfo
	;mov edx , offset CCStrno
	;mov eax, CCSize
	;mov countOfNumberEntered , eax
	mov edi , offset credit_card_info
	mov edx , offset CCStrno
	mov ecx , cridetInfoLength
	mov esi , 0        ;count to get true number

    searchByNumberLoop:
	mov al , [edi]
	mov bl , [edx]
	cmp al , bl
	je equallnum
	jne notequallnum
	equallnum:
		inc edi
		inc edx
		inc esi
		jmp nextnum
	notequallnum:
		inc edi
		cmp esi , 0
		jne newInitializeForNum
		je nextIteration
	newInitializeForNum:
		mov esi , 0
		mov edx , offset CCStrno
	nextnum:
		cmp esi , countOfNumberEntered
		je compareForComma
		jne nextIteration
		compareForComma:
			;inc edi 
			mov edx , offset CCStrno
			mov al , [edi]
			cmp al , ","
			je _numberFound
		nextIteration:
	loop searchByNumberLoop
	;mwrite<"Number not found" , 0dh , 0ah >
	jmp _numNotFound
	_numberFound:
		mov foundNumberBool , 1
	JMP endFunctionSearchByNumber
	_numNotFound:
		mov foundNumberBool, 0
	endFunctionSearchByNumber:
;popad
ret
FunctionSearchByNumber ENDP
;--------------------------------------------------------------------------------
;Returns: credit_Card_Info contains the whole file
;         cridetInfoLength = num Of Bytes Read
; use variables: cridetInfoLength , credit_card_info , nameChoice , choice , CCStrno , fileHandle ,
;                countOfNumberEntered , holderName , countOfNameEntered , newAmount
; use functions OpenAndTruncationPaymentFileToWrite ,call MoveCursorToEnd , ClosPaymentFile
;--------------------------------------------------------------------------------
TopUpBalance proc 
	pushad
	CALL ReadFileInCreditInfo
	;search in credit card info file with name or number
	mwrite<"Search by number or name?", 0dh , 0ah >
	mov edx , offset choice
	mov ecx , lengthof choice
	call readstring

	mov ecx , eax
	mov esi , offset nameChoice
	mov edi , offset choice;entered choice(name/number)
	cld
	repe cmpsb
	je searchByName
	mov esi , offset numberChoice
	mov edi , offset choice;entered choice(name/number)
	cld
	repe cmpsb
	je searchByNumber
	jmp invalidChoice
	searchByNumber:
	mwrite<"Enter the number" , 0dh , 0ah >
		mov edx , offset CCStrno
		mov ecx , lengthof CCStrno
		call readstring
		mov countOfNumberEntered , eax
		mov CCsize , eax
		call CreditCardVAlidation
		cmp CCRemainder , 0
		jne notValidCC
		;; law 3mlt run bel name abl number bfdy l haga el fel name 
		mov ecx , holderNameSize
		mov edi , offset holderName
		mov al , ' '
		rep stosb
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		call FunctionSearchByNumber
		cmp foundNumberBool , 1
		je numberFound
		notValidCC:
		;mwrite<"Number not found" , 0dh , 0ah >
		INVOKE MessageBox,NULL,ADDR errorCCN,ADDR caption1,MB_OK+MB_ICONSTOP
		;bonus
		MOV EDI, OFFSET invaliduserRecord
		MOV ESI, OFFSET ccstrno
		MOV ECX, lengthof ccstrno
		CLD
		rep MOVSB
		INC EDI

		MOV ESI, offset invtopupstr
		MOV ECX, Lengthof invtopupstr
		CLD
		rep MOVSB

		MOV ESI, OFFSET errorCCN
		MOV ECX, lengthof errorCCN
		CLD
		rep MOVSB
	CALL AddinvalidRecordToFile	
	popad
	ret
	searchByName:
		mwrite<"Enter the name" , 0dh , 0ah >
		mov edx , offset holderName
		mov ecx , lengthof holderName
		call readstring

		mov countOfNameEntered , eax
		mov holderNameSize , eax
		;; law 3mlt run bel number abl name bfdy l haga el fel number 
		mov ecx , CCsize
		mov edi , offset CCStrno
		mov al , ' '
		rep stosb
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		call FunctionSearchByName
		cmp foundNameBool , 1
		je nameFound
		;mwrite<"Name not found" , 0dh , 0ah >
		INVOKE MessageBox,NULL,ADDR errorname,ADDR caption1,MB_OK+MB_ICONSTOP
		;bonus
		MOV EDI, OFFSET invaliduserRecord
		MOV ESI, OFFSET holdername
		MOV ECX, lengthof holdername
		CLD
		rep MOVSB
		INC EDI

		MOV ESI, offset invtopupstr
		MOV ECX, Lengthof invtopupstr
		CLD
		rep MOVSB

		MOV ESI, OFFSET errorname
		MOV ECX, lengthof errorname
		CLD
		rep MOVSB
	CALL AddinvalidRecordToFile	
	popad
	ret
	numberFound:
		add edi , 34 ; edi wa2f 3la awl rakam fel amount fel rakam el l2ah
		;; b7ot l number fel valid
		push edi 
		push esi
		push ecx
		;; reinitialize validuserRecord
		mov ecx , lengthof validuserRecord
		sub ecx , 2
		mov edi , offset validuserRecord
		mov al , ' '
		rep stosb

		MOV EDI, OFFSET validuserRecord
		MOV ECX, CCsize
		MOV ESI, OFFSET CCStrno
		CLD
		rep MOVSB
		pop ecx
		pop esi
		pop edi
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		jmp afterMoveEdiToAmount
	nameFound:
		mov eax,21  ; fixed length of name + ","
		sub eax,esi ; btra7 l count bta3 l name char mn l fixed length bta3 l name
		add edi , eax ; edi wa2f 3la awl rakam fel amount fel esm el l2ah

		;; b7ot l name fel valid
		push edi 
		push esi
		push ecx
		;; reinitialize validuserRecord
		mov ecx , lengthof validuserRecord
		sub ecx , 2
		mov edi , offset validuserRecord
		mov al , ' '
		rep stosb

		MOV EDI, OFFSET validuserRecord
		MOV ECX, holderNameSize
		MOV ESI, OFFSET holdername
		CLD
		rep MOVSB
		pop ecx
		pop esi
		pop edi
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	afterMoveEdiToAmount:
		mwrite<"Enter new amount" , 0dh , 0ah >
		call readint
		mov newAmount , eax
		MOV topupAmountstr, EAX

		mov ecx , 10
		copyLoopToGetCountOfDigit:
			mov al , [edi]
			cmp al , ' '
			je outTheLoop
			inc amountCount
			inc edi
		loop copyLoopToGetCountOfDigit
		outTheLoop:
		mov ecx , amountCount
		sub edi , amountCount
		mov edx , edi 
		mov esi , offset amountInFile
		call chartoint
		sub esi , 4    ; wa2f 3la a5r rakam
		mov ebx , [esi]
		mov ecx , amountCount
		sub ecx , 1
		sub esi , 4 
		cmp ecx , 0
		je editAmount
		convertAmountToInt:
			mov eax , [esi]
			mul tenMultiple
			add ebx , eax
			sub esi , 4
			mov eax , tenMultiple
			mov edx , 10
			mul edx
			mov tenMultiple , eax
		loop convertAmountToInt

		editAmount:
		mov amountInFileInt , ebx
		mov eax , newAmount
		add eax , amountInFileInt
		mov newAmount , eax
		mov esi , offset amountArray
		mov ecx , 10
		;call intToChar
	intToCharLoopAmount:
		MOV EDX, 0;'el remainder' initialize it every time
		MOV EBX, 10; el maqam
		DIV EBX
		OR EDX, 00110000b;take the remainder to convert it to char
		MOV [ESI], DL;3shan el string ely byshawer 3lih el ESI no3o BYTE
		CMP EAX, 0;3shan el rkm b3d ma etra7 3dad el digits momkn tkon 2a2l mn el asly ele fl file
		JE _outtLoop 
		ADD ESI, 1;3shan lama ytl3 mn  el loop yb2a wa2f 3la a5r el rkm

	LOOP intToCharLoopAmount
		
		_outtLoop:

		sub ecx , 1 
		mov eax , 10
		sub eax , ecx   ; num of digit of new amount
		mov amountCount , eax
		;mov esi , offset amountArray
		mov ecx , amountCount
		copyLoop:
			mov al , [esi]
			mov [edi] , al
			dec esi
			inc edi
		loop copyLoop

	call OpenAndTruncationPaymentFileToWrite
	mov fileHandle , eax
	call MoveCursorToEnd

	mov eax , fileHandle
	mov edx , offset credit_Card_Info
	mov ecx , cridetInfoLength
	call WriteToPaymentFile
	mov eax , fileHandle
	call ClosPaymentFile
	;mwrite<"DONE" , 0dh , 0ah >
	INVOKE MessageBox,NULL,ADDR INFOMSG2,ADDR caption2,MB_OK+MB_ICONINFORMATION

	;;;;;;;;;;;;;;;;;;;;;;;;write in valid log file;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	comment @
	;re initialize
		MOV EDI, OFFSET validuserRecord
		MOV AL, ' '
		MOV ECX, lengthof validuserrecord
		_LABEL2:
		MOV [EDI], AL
        INC EDI
		LOOP _label2

	
	
	;for bonus msh 3arfa tet7at hna name wla cc hanshof
	MOV EDI, OFFSET validuserRecord

	MOV ECX, CCsize
	MOV ESI, OFFSET CCStrno
	CLD
	rep MOVSB
	;;;;;;;;;;;;;;

	MOV ECX, holderNameSize
	MOV ESI, OFFSET holdername
	CLD
	rep MOVSB
	@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#

	MOV pointer, 2;ana 7ateet value to indicate later ene gaya mn el function pay
	CALL AddvalidRecordToFile
	;; reinitialize tuopUpBalance data
	mov tenMultiple , 10
	mov newAmount , 0
	mov amountCount , 0
	popad
	ret
	;;;;;;;;;;;;;;;;;;;;;;;;write in valid log file;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	invalidChoice:
	INVOKE MessageBox,NULL,ADDR invalidChoiceMsg,ADDR caption1,MB_OK+MB_ICONSTOP
	popad
	ret 
TopUpBalance ENDP

;--------------------------------------------------------------------------------
;Returns: credit_Card_Info_NEW contains the new updated file
;         cridetInfoLength = num Of Bytes Read
; use variables: cridetInfoLength , credit_card_info,credit_card_info_new , nameChoice , choice , fileHandle ,
;                 holderName , countOfNameEntered 
; use functions OpenAndTruncationPaymentFileToWrite ,WriteToPaymentFile,call MoveCursorToEnd , ClosPaymentFile
;--------------------------------------------------------------------------------
DELETERECORD PROC 
;search in credit card info file with name or number
	CALL ReadFileInCreditInfo
	mwrite<"DELETE by number or name?", 0dh , 0ah >
	mov edx , offset choice
	mov ecx , lengthof choice
	call readstring

	mov ecx , eax
	mov esi , offset nameChoice
	mov edi , offset choice
	cld
	repe cmpsb
	je DELBYNAME
	mov esi , offset numberChoice
	mov edi , offset choice;entered choice(name/number)
	cld
	repe cmpsb
	je DELBYNUMBER
	jmp invalidchoiceDELETE

	DELBYNAME:
	CALL DELETENAME
	jmp FINI

	DELBYNUMBER:
	CALL DELETENUMBER
	jmp FINI
	invalidchoiceDELETE:
	INVOKE MessageBox,NULL,ADDR invalidChoiceMsg,ADDR caption1,MB_OK+MB_ICONSTOP

	FINI:
	RET
DELETERECORD ENDP

;--------------------------------------------------------------------------------
;Returns: credit_Card_Info_NEW contains the new updated file
;         cridetInfoLength = num Of Bytes Read
; use variables: cridetInfoLength , credit_card_info,credit_card_info_new , nameChoice , choice , fileHandle ,
;                 holderName , countOfNameEntered 
; use functions OpenAndTruncationPaymentFileToWrite ,WriteToPaymentFile,call MoveCursorToEnd , ClosPaymentFile
;--------------------------------------------------------------------------------
DELETENAME PROC
mwrite<"Enter the name" , 0dh , 0ah >
	mov edx , offset holderName
	mov ecx , lengthof holderName
	call readstring
	mov countOfNameEntered , eax

	;OFFSET ON THE READ FILE
	mov edi , offset credit_Card_Info
	;OFFSET ON THE NAME ENTERED
	mov edx , offset holderName
	;THE ACUTUAL LENGTH OF RECORDSS
	mov ecx , cridetInfoLength
	;count to get true name
	mov esi , 0       
	searchByNameLoop:
	mov al , [edi]
	mov bl , [edx]
	cmp al , bl
	je equallchar
	jmp notequall
	equallchar:
	inc esi 
	inc edx
	inc edi
	jmp next
	notequall:
	inc edi
	cmp esi , 0
	jg newIntialize
	jmp next
	newIntialize:
	mov esi , 0
	mov edx , offset holderName
	next:
	cmp esi , countOfNameEntered
	je nameFound
	loop searchByNameLoop
	;mwrite<"Name not found" , 0dh , 0ah >
	INVOKE MessageBox,NULL,ADDR errorname,ADDR caption1,MB_OK+MB_ICONSTOP
	RET
	nameFound:;HENA ANA BZABT AL EDI B7ETH ENO YSHAWR 3la awel al record
	SUB edi , ESI
	SUB edi , 29
		
	MOV EDX,OFFSET credit_Card_Info
	MOV ESI,OFFSET credit_Card_Info_NEW

	mov ECX,cridetInfoLength
	copyLoop:
		;im going to compare the offset edx with edi 
		cmp EDX,EDI
		JE IGNOREE
		mov al , [edx]
		mov [ESI] , al
		inc edx
		inc esi
		jmp complete
		IGNOREE:
		ADD EDX,62
		SUB ECX,61
		complete:
	loop copyLoop

	call OpenAndTruncationPaymentFileToWrite
	mov fileHandle , eax
	call MoveCursorToEnd

	mov eax , fileHandle
	mov edx , offset credit_Card_Info_new
	mov ecx , cridetInfoLength
	SUB ECX,62
	call writestring
	call crlf
	call WriteToPaymentFile
	mov eax , fileHandle
	call ClosPaymentFile              
	RET
DELETENAME ENDP

;--------------------------------------------------------------------------------
;Returns: credit_Card_Info_NEW contains the new updated file
;         cridetInfoLength = num Of Bytes Read
; use variables: cridetInfoLength , credit_card_info,credit_card_info_new , nameChoice , choice , fileHandle ,
;                 holderName , countOfNameEntered 
; use functions OpenAndTruncationPaymentFileToWrite ,WriteToPaymentFile,call MoveCursorToEnd , ClosPaymentFile
;--------------------------------------------------------------------------------
DELETENUMBER PROC
	mwrite<"Enter the NUMBER" , 0dh , 0ah >

	mov edx , offset NUMBEREntered
	mov ecx , lengthof NUMBEREntered
	call readstring
	mov countOfNUMBEREntered , eax

	;OFFSET ON THE READ FILE
	mov edi , offset credit_Card_Info
	;OFFSET ON THE NAME ENTERED
	mov edx , offset NUMBEREntered
	;THE ACUTUAL LENGTH OF RECORDSS
	mov ecx , cridetInfoLength
	;count to get true name
	mov esi , 0       
	searchByNUMBERLoop:
	mov al , [edi]
	mov bl , [edx]
	cmp al , bl
	je equallchar
	jmp notequall
	equallchar:
	inc esi 
	inc edx
	inc edi
	jmp next
	notequall:
	inc edi
	cmp esi , 0
	jg newIntialize
	jmp next
	newIntialize:
	mov esi , 0
	mov edx , offset nUMBEREntered
	next:
	cmp esi , countOfNUMBEREntered
	je nUMBERFound
	loop searchByNUMBERLoop
	;mwrite<"Number not found" , 0dh , 0ah >
	INVOKE MessageBox,NULL,ADDR errorCCN,ADDR caption1,MB_OK+MB_ICONSTOP
	RET
	nUMBERFound:;HENA ANA BZABT AL EDI B7ETH ENO YSHAWR 3la awel al record
	SUB edi , ESI
	;SUB edi , 29
		
	MOV EDX,OFFSET credit_Card_Info
	MOV ESI,OFFSET credit_Card_Info_NEW

	mov ECX,cridetInfoLength
	copyLoop:
	;im going to compare the offset edx with edi 
	cmp EDX,EDI
	JE IGNOREE
	mov al , [edx]
	mov [ESI] , al
	inc edx
	inc esi
	jmp complete
	IGNOREE:
	ADD EDX,62
	SUB ECX,61
	complete:
	loop copyLoop

	call OpenAndTruncationPaymentFileToWrite
	mov fileHandle , eax
	call MoveCursorToEnd

	mov eax , fileHandle
	mov edx , offset credit_Card_Info_new
	mov ecx , cridetInfoLength
	SUB ECX,62
	call writestring
	call crlf
	call WriteToPaymentFile
	mov eax , fileHandle
	call ClosPaymentFile              

RET
DELETENUMBER ENDP

readingUserInfotoAdd PROC		
	;;;;;;;;;;;;;;;;;;;;;;;;read info;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	MOV EDI, offset userRecord; to add all user info in one string

	MOV EDX, OFFSET CCStrno
	MOV ECX, LENGTHOF CCStrno

	mwrite<"Enter credit card number:",0dh, 0ah >
	CALL READSTRING 
	MOV ccsize, eax

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	MOV EDX, OFFSET expiryDate
	MOV ECX, LENGTHOF expiryDate
	mwrite<"Enter Expiry Date:",0dh, 0ah >

	CALL READSTRING
	MOV expiryDateSize, EAX
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	MOV EDX, OFFSET Cvv
	MOV ECX, LENGTHOF CVV
	mwrite<"Enter CVV number:",0dh, 0ah >

	CALL READSTRING
	MOV cvvSize, EAX

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	MOV EDX, OFFSET holderName
	MOV ECX, LENGTHOF holderName
	mwrite<"Enter Your name:",0dh, 0ah >
	CALL READSTRING
	MOV holderNameSize, EAX

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MOV EDX, OFFSET balance
	MOV ECX, LENGTHOF balance
	mwrite<"Enter Your Balnace:",0dh, 0ah >

	CALL READSTRING
	MOV balanceSize, EAX

	;;;;;;;;;;;;;;;;;;;;;;;;end read info;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	CMP CvvSize, DWORD PTR 3
	JNE cvvSizeNotValid

	MOV ECX, CCSIZE
	CALL creditCardValidation
	CMP CCRemainder, 0
	JE ValidCCnum
		;mwrite<"Not valid credit card number",0dh , 0ah >
		INVOKE MessageBox,NULL,ADDR invalidCCN,ADDR caption1,MB_OK+MB_ICONSTOP
	ret 
				
				
	ValidCCnum:
	;mwrite<"Added successfuly">
	
	;adding the new entered string in userRecord then in file
	MOV ECX, CCsize
	MOV ESI, OFFSET CCStrno
	MOV EDI, OFFSET userRecord
	CLD
	rep MOVSB



	MOV ECX, 16
	SUB ECX, CCSize

	;adding spaces
	CLD
	MOV al, ' '
	REP STOSB

	MOV AL, ','
	MOV [EDI], AL
	ADD EDI, TYPE userrecord

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	MOV ECX, expiryDateSize
	MOV ESI, OFFSET expiryDate
	CLD
	rep MOVSB
	
	MOV ECX, 7
	;MOV EDX, LENGTHOF expiryDate
	SUB ECX, expiryDateSize
	CLD
	MOV AL, ' '
	REP STOSB

	MOV AL, ','
	MOV [EDI], AL
	ADD EDI, TYPE userrecord
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	MOV ECX, CVVsize
	MOV ESI, OFFSET cvv
	CLD
	rep MOVSB
	;No need to add spaces
	MOV AL, ','
	MOV [EDI], AL
	ADD EDI, TYPE userrecord
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   MOV ECX, holdernameSize

	MOV ESI, OFFSET holdername
	CLD
	rep MOVSB

	MOV ECX, 20
	SUB ECX, holdernameSize

	
	CLD
	MOV AL, ' '
	REP STOSB
	MOV AL, ','
	MOV [EDI], AL
	ADD EDI, TYPE userrecord

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	MOV ECX, balanceSize
	MOV ESI, OFFSET balance
	CLD
	rep MOVSB

	MOV ECX, 10
	SUB ECX, balanceSize
	MOV AL, ' '
	CLD
	REP STOSB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;for bonus
	MOV ECX, CCsize
	MOV ESI, OFFSET CCStrno
	MOV EDI, OFFSET validuserRecord
	CLD
	rep MOVSB
	;;;;;;;;;;;;;;

	MOV ECX, holdernameSize
	MOV ESI, OFFSET holdername
	CLD
	rep MOVSB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MOV EAX , holderNameSize							; EDIT NOURHAN
	MOV countOfNameEntered , eax						; EDIT NOURHAN
	CALL FunctionSearchByName 
	CMP foundNameBool, 0; not found
	JNE nameExists
	MOV EAX , CCSize									; EDIT NOURHAN	
	MOV countOfNumberEntered , EAX						; EDIT NOURHAN
	CALL FunctionSearchByNumber
	CMP foundNumberBool, 0 ;not found
	JNE CCExists
	CALL AddRecordToFile
	JMP done

	cvvSizeNotValid:
		;mwrite<"Adding credit card failed incorrect cvv size!!",0dh ,0ah >
		INVOKE MessageBox,NULL,ADDR invaildCCV,ADDR caption1,MB_OK+MB_ICONSTOP
		JMP done
	nameExists:
		;mWrite<"Name already exists!!",0dh ,0ah >
		INVOKE MessageBox,NULL,ADDR existname,ADDR caption1,MB_OK+MB_ICONSTOP
		JMP done
	CCExists:
		;mWrite<"Credit card number already exists!!",0dh ,0ah >
		INVOKE MessageBox,NULL,ADDR existCCN,ADDR caption1,MB_OK+MB_ICONSTOP


	done:
	
RET
readingUserInfotoAdd ENDP

creditCardValidation PROC
PUSHAD
	;loops on every character in the cc string and converts it to int and puts it in to the cc int array
	MOV EDX, OFFSET CCStrno;!!
	MOV ESI, OFFSET CCIntno; array

	MOV ECX, CCSize
	cmp CCsize, 16
	JA notValid
	CALL charToInt

	;to check if it begins by 4 or 5
	MOV EBX, OFFSET CCIntno
	CMP [EBX], DWORD PTR 4
	JE checkSize
    CMP [EBX], DWORD PTR 5
	JNE notValid


	checkSize:
	;to check size of cc string
	CMP CCSize, DWORD PTR 13
	JE contValidation
	CMP CCSize, DWORD PTR 14
	JE contValidation
	CMP CCSize, DWORD PTR 15
	JE contValidation
	CMP CCSize, DWORD PTR 16
	JE contValidation
	JMP notValid


	contValidation:
	;msh ha7tag da el esi rag3le mn el fn ele ably wa2f 3la nhayt el array
	MOV ECX, CCSize
	SHR ECX, 1 ;asmna 3la el 2 3shan bmshy 3la nos el arkam according to LUHN ALGO
	SUB ESI, 8; keda howa wa2f 3la el element el 2bl el a5ir f-el array(according to LUHN ALGORITHM)

	doubling:
	MOV EDI, [ESI]
	ADD [ESI], EDI


	addingTwoDigitNumber:;3ayza ashof de htgm3 sa7 wala l2
	MOV EDX, 0
	MOV EAX, [ESI]
	MOV EBX, 10
	DIV EBX
	ADD EAX, EDX; adding quotient+remainder
	MOV [ESI], EAX; bnrg3o mkano f-el array

	continue:

	SUB ESI, 8 ;bamshy 3la wa7d w aseeb wa7d w el array no3ha dword
	LOOP doubling

	MOV edx,0 ;for sum
	MOV ECX, CCSize
	MOV ESI, OFFSET CCIntno ;bamshy mn awlo mna kont 7atet kol wa7d fmkano b3d matzabt

	addingAllDigits:

	ADD EDX, [ESI]
	add esi, type ccintno
	LOOP addingAllDigits

	MOV EAX, EDX; move sum to EAX
	MOV EDX, 0
	MOV EBX, 10
	DIV EBX

	MOV CCRemainder, EDX

	notValid:

     POPAD
	RET
creditCardValidation ENDP
;converts what in edx to integers and put them back in esi
charToInt PROC

	charToIntLoop:
		MOV EBX, [EDX]
		AND  EBX , BYTE ptr 11001111b
		MOV [ESI], EBX
		ADD ESI, 4
		ADD EDX, 1
	LOOP charToIntLoop  

  RET
charToInt ENDP

;converts what in ebx "a DWORD number" to chars "BYTE" and put it back in esi

intToChar PROC;3aiza azbt el ecx 2bl ma 2nady 3liha
PUSHAD
	MOV EAX, EBX; holds the number at the beginning
	intToCharLoop:
		;3aiza 2ml mod ll ebx 3shan 25od kol digit w 27welha l char
		MOV EDX, 0;'el remainder' initialize it every time
		MOV EBX, 10; el maqam
		DIV EBX
		OR EDX, 00110000b;take the remainder to convert it to char
		MOV [ESI], DL;3shan el string ely byshawer 3lih el ESI no3o BYTE

		CMP EAX, 0;3shan el rkm b3d ma etra7 3dad el digits momkn tkon 2a2l mn el asly ele fl file
		JE _outt 
		ADD ESI, 1;3shan lama ytl3 mn  el loop yb2a wa2f 3la a5r el rkm

	LOOP intToCharLoop
	_outt: ;m7tageen n3dl el string w nzwed b ba2e el ecx msafat
		
		SUB ECX, 1; 3shan 3mlna break mn el loop 2bl ma el ecx yn2s f a5r lafa
		SUB digitscounter, ECX;number of digits of the new balance
		MOV EAX, digitsCounter

		; 3shan a5ly el esi wa2ef 3la2wl element
		SUB ESI, digitscounter
		INC ESI
	
		;kda w2ft el edx 3la a5r element
		MOV EDX, ESI
		ADD EDX, digitscounter
		SUB EDX, 1

		;b3dl el string
	    MOV ECX, digitscounter;loop b 3dd digits el balnce el gdeed
		SHR ECX, 1; 3shan ymshy 3la nos el string
		CMP ECX, 0;3shan law howa digit wa7da may3mlsh reOrder   ;ADDED
		JE _dontReOrderStr                                       ;ADDED
		_reorderStr:

			MOV BL, [ESI]
			XCHG BL, [EDX]
			XCHG BL, [ESI]
			INC ESI
			DEC EDX

		LOOP _reorderStr

		_dontReOrderStr:                       ;ADDED

		;kda braga3 el ESI l awl el string 
		MOV ESI, OFFSET balanceToEditSTR

		_continueToAddSpaces:                  ;ADDED
		;kda wa2f 3la b3d el string b 1 
		ADD ESI, digitscounter

		 ;adding spaces
		 ;kda m3aya el esi wa2f 3la b3d a5r digit 3yza a7ot spaces bl b2y
		 MOV ECX, 10
		 SUB ECX, digitscounter
		 MOV AL, ' '

		 addspaces:
			 MOV [ESI], AL
			 INC ESI
		 LOOP addspaces

		SUB ESI, 10 ;3shan arg3o 3la awl el string
POPAD
ret
intToChar ENDP

checkPayAmount PROC
 ;re initialize data
	MOV tenMultiple, 10
	MOV digitsCounter, 0
	MOV balanceToEditInt, 0
	;;;;;;;;;;;;;;;;;;;;;;;;;
	CALL ReadFileInCreditInfo
	mov edx , offset holderName
    mov eax, holderNameSize
	mov countOfNameEntered ,eax

	;OFFSET ON THE READ FILE
	mov edi , offset credit_Card_Info
	;OFFSET ON THE NAME ENTERED
	mov edx , offset holderName
	;THE ACUTUAL LENGTH OF RECORDSS
	mov ecx , cridetInfoLength
	;count to get true name
	mov esi , 0       
	_searchByNameLoop:
		mov al , [edi]
		mov bl , [edx]
		cmp al , bl
		je _equallchar
		jmp _notequall
		_equallchar:
		inc esi 
		inc edx
		inc edi
		jmp _next
		_notequall:
		inc edi
		cmp esi , 0
		jg _newIntialize
		jmp _next
		_newIntialize:
		mov esi , 0
		mov edx , offset holderName
		_next:
		cmp esi , countOfNameEntered
		je _nameFound
	loop _searchByNameLoop

	;mwrite<"Name not found" , 0dh , 0ah >
	INVOKE MessageBox,NULL,ADDR errorname,ADDR caption1,MB_OK+MB_ICONSTOP
		;bonus
		MOV EDI, OFFSET invaliduserRecord
		MOV ESI, OFFSET holdername
		MOV ECX, lengthof holdername
		CLD
		rep MOVSB
		INC EDI

		MOV ESI, offset invpaystr
		MOV ECX, Lengthof invpaystr
		CLD
		rep MOVSB

		MOV ESI, OFFSET errorname
		MOV ECX, lengthof errorname
		CLD
		rep MOVSB
	CALL AddinvalidRecordToFile	
	RET
	_nameFound:;HENA ANA BZABT AL EDI B7ETH ENO YSHAWR 3la awel al record
	MOV EAX,20
	SUB EAX, holderNameSize
	inc EAX
	ADD edi , eax; keda howa wa2ef 3la 2wel digit f-el balance
	MOV EDX, offset balanceToEditSTR
		
	MOV ECX, 10 ;take the balance in eax
	MOV EBX, 0
	_CopyBalance:;ba2ra mn el file el balance

		MOV AL, [EDI]
		CMP AL, ' '
		JE _outtLoop
		MOV [EDX], AL
		INC EDX
		INC EDI
		;ADD digitsCounter, 1; to know the number of digits of balance
		ADD  EBX, 1
	LOOP _CopyBalance

	_outtLoop:

	MOV digitsCounter, EBX
	MOV EDX, OFFSET balanceToEditSTR
	MOV ESI, offset balanceToEditARR
	MOV ECX, digitsCounter
	CALL chartoint

	MOV ECX, digitscounter
	DEC ECX ;3shan ana kda mshya 3la el elements -1

	;convert the numbers in array to a number
	SUB ESI, TYPE balanceToEditARR ;kda el esi rag3le mn charToInt bases bara el array fa brg3o l 2a5r wa7d
	MOV EBX, [ESI];initialy ebx holds the last element 3shan dh msh hydrb f 7aga to sum the digits in it
	;lw ecx b zero yb2a 1 digit yb2a msh ha7tag a7wlo
	cmp ECX, 0
	JE onedigitnumber


	SUB ESI, TYPE balanceToEditARR ;kda el esi bases 3la el element el 2bl el a5ir
	_convertToIntLoop:
		;to multiply by 10 
			MOV EAX, [ESI]
			MUL tenMultiple		
			ADD EBX, EAX; holds the balance in file at the end
			SUB ESI, TYPE balanceToEditARR
			;3shan kol mara adrb el tenMultiple * 10 3shan a3rf agm3 el digits s7
			MOV EAX, tenMultiple
			MOV EDX, 10
			MUL EDX
			MOV tenMultiple, EAX
		LOOP _convertToIntLoop

	   onedigitnumber:

		MOV balanceToEditInt, EBX; the balance in the file to be checked

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		CMP paymentAmount, EBX ;EBX: balanceToEditInt, because I can't CMP 2 memories
		JA _notEnoughMoney
		;else: enough money
		SUB EBX, paymentAmount ;holds the new amount after payment
		;SUB EDI, (digitsCounter*TYPE balanceToEditInt);3shan yrg3 yo2f 3la bedayt el balance f-el file
		
		;bdl el satr ely fo2
		;3shan 23rf 2n2s mn el EDI 3dd el bytes s7 w 2w2f el EDI 3la bedayt el balance f-el file		
		SUB EDI, digitsCounter;3shan yrg3 yo2f 3la bedayt el balance f-el file
		
		MOV ESI, OFFSET balanceToEditSTR ;to hold the new balance after call the function
		MOV ECX, digitsCounter ;fe aswa2 7ala haymshe digits cout 3shan momkn lw tar7t 3dad el digits y2l
		CALL intToChar ;el mafrod el ESI rag3 mn hna wa2ef 3la 2wl el string ely hktbo f-el file
		
		JMP _donePayment
		_notEnoughMoney:
		;mwrite<"There is not sufficent money", 0ah,0dh >
		INVOKE MessageBox,NULL,ADDR errorpay,ADDR caption1,MB_OK+MB_ICONSTOP
		;bonus
		MOV EDI, OFFSET invaliduserRecord
		MOV ESI, OFFSET ccstrno
		MOV ECX, lengthof ccstrno
		CLD
		rep MOVSB
		INC EDI

		MOV ESI, offset invpaystr
		MOV ECX, Lengthof invpaystr
		CLD
		rep MOVSB

		MOV ESI, OFFSET errorpay
		MOV ECX, lengthof errorpay
		CLD
		rep MOVSB
	CALL AddinvalidRecordToFile
		JMP _faild
		_donePayment:		
		;mwrite<"Successful Payment", 0ah,0dh >
		INVOKE MessageBox,NULL,ADDR infomsg0,ADDR caption2,MB_OK+MB_ICONINFORMATION
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;write the new balance in the string ele shayl el file kolo
		MOV ECX, 10
		editInPlace:
			MOV AL, [ESI]
			MOV [EDI], AL
			ADD ESI, 1
			ADD EDI, 1
		LOOP editInPlace
	
		MOV EDX, OFFSET credit_Card_Info

		call OpenAndTruncationPaymentFileToWrite
		mov fileHandle , eax
		call MoveCursorToEnd
		MOV EDX, OFFSET credit_Card_Info

		mov eax , fileHandle
		mov ecx , cridetInfoLength
		call writestring
		call crlf
		
		MOV EDX, OFFSET credit_Card_Info
		
		call WriteToPaymentFile
		mov eax , fileHandle
		call ClosPaymentFile 


		;;;;;;;;;;;;;;;;;;;;;;;;write in valid log file;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		MOV pointer, 1;ana 7ateet value to indicate later ene gaya mn el function pay
	    CALL AddvalidRecordToFile	
	    ;;;;;;;;;;;;;;;;;;;;;;;;write in valid log file;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



	_faild:



	ret
checkPayAmount ENDP

payByCC PROC
PUSHAD

	MOV EDX, offset CCStrno
	MOV ECX, 20;LENGTHOF CCStrno

	mwrite<"Enter Credit Card number:",0dh,0ah >
	CALL readString
	MOV CCSize, EAX

	MOV EDX, OFFSET expiryDate
	MOV ECX, LENGTHOF expiryDate
	mwrite<"Enter expiry date:",0dh,0ah >
	CALL readString
	MOV expiryDateSize, EAX

	
	MOV EDX, OFFSET CVV
	MOV ECX, LENGTHOF CVV
	mwrite<"Enter CVV:",0dh,0ah >
	CALL readString
	MOV cvvSize, EAX

	
	MOV EDX, OFFSET holderName
	MOV ECX, LENGTHOF holderName
	mwrite<"Enter your name:",0dh,0ah >
	CALL readString
	MOV holderNameSize, EAX

	mwrite<"Enter payment amount:",0dh,0ah >
	CALL READINT
	MOV paymentAmount, EAX
	MOV paymentAmountstr, EAX
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end reading;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;for bonus
	MOV ECX, CCsize
	MOV ESI, OFFSET CCStrno
	MOV EDI, OFFSET validuserRecord
	CLD
	rep MOVSB
	;;;;;;;;;;;;;;

	MOV ECX, holdernameSize
	MOV ESI, OFFSET holdername
	CLD
	rep MOVSB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call creditCardValidation

	CMP CCRemainder, 0
	JE 	checkPayCVV
	;mWrite<"Not Valid CC!!",0dh , 0ah >
	INVOKE MessageBox,NULL,ADDR invalidCCN,ADDR caption1,MB_OK+MB_ICONSTOP
	;bonus
		MOV EDI, OFFSET invaliduserRecord
		MOV ESI, OFFSET ccstrno
		MOV ECX, lengthof ccstrno
		CLD
		rep MOVSB
		INC EDI

		MOV ESI, offset invpaystr
		MOV ECX, Lengthof invpaystr
		CLD
		rep MOVSB

		MOV ESI, OFFSET invalidCCN
		MOV ECX, lengthof invalidCCN
		CLD
		rep MOVSB
	CALL AddinvalidRecordToFile
	JMP cont

	checkPayCVV:
	; check cvv length
	CMP cvvSize, 3
	JNE notValidPaycvvSize
	CALL checkPayAmount
	JMP cont

	notValidPaycvvSize:
		;mWrite<"Not valid transaction due to CVV", 0dh, 0ah >
		INVOKE MessageBox,NULL,ADDR invaildCCV,ADDR caption1,MB_OK+MB_ICONSTOP
		;bonus
		MOV EDI, OFFSET invaliduserRecord
		MOV ESI, OFFSET ccstrno
		MOV ECX, lengthof ccstrno
		CLD
		rep MOVSB
		INC EDI

		MOV ESI, offset invpaystr
		MOV ECX, Lengthof invpaystr
		CLD
		rep MOVSB

		MOV ESI, OFFSET invaildCCV
		MOV ECX, lengthof invaildCCV
		CLD
		rep MOVSB
	CALL AddinvalidRecordToFile
	cont:
POPAD
ret
payByCC ENDP


;4136233030479741
;Addison Gonzalez
;--> 2000$.

;5168132919681062
; Madison Parker
; -->3312$


;  5168132919681062  ,11/2019  , 550  ,  Madison Parker      ,  3312      

;#####################################################################
MAIN PROC															;#
																	;#
	PROGLOOP:														;#
	MWRITE<"Please choose an operation:",0DH,0AH >					;#
	MWRITE<"[1] For adding a client to file",0DH,0AH >				;#
	MWRITE<"[2] For top up the amount of balance",0DH,0AH >			;#
	MWRITE<"[3] For Payment/Making a transtion",0DH,0AH >			;#
	MWRITE<"[4] Deleting a client from the file",0DH,0AH >			;#
	MWRITE<"[0] To Exit",0DH,0AH >									;#
																	;#
	CALL READDEC													;#
	CMP EAX,0														;#
	JE FIN															;#
	CMP EAX, 1														;#
	JE _ADD															;#
	CMP EAX, 2														;#
	JE _TOPUP														;#
	CMP EAX, 3														;#
	JE _PAYBY														;#
	CMP EAX, 4														;#
	JE _DELETE														;#
																	;#
	JMP WRONG														;#
																	;#
	_ADD:															;#
	CALL readingUserInfotoAdd										;#
	JMP CONT														;#
																	;#
	_TOPUP:															;#
	CALL TopUpBalance												;#
	JMP CONT														;#
																	;#
	_PAYBY:															;#
	CALL payByCC													;#
	JMP CONT														;#
																	;#
	_DELETE:														;#
	CALL DELETERECORD												;#
	JMP CONT														;#
																	;#
	WRONG:															;#
	MWRITE<"INVALID NUMBER",0DH,0AH >								;#
																	;#
	CONT:															;#
	JMP PROGLOOP													;#
																	;#
	FIN:															;#
																	;#
	EXIT															;#
MAIN ENDP															;#
;#####################################################################
END main

;;;;;;;;;finaal;;;;;;;;;;;