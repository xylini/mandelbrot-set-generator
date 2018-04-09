;Jakub Pajor WIEiT 2017
;Mandelbrot set generator
;sample input: mansetgen.exe -2.0 1.0 -1.0 1.0

.286

data segment
	argv db 200 dup ('$')
	argb db 10 dup ('$') 	;przechowuje ilosc znakow wraz z poczatkowa spacja
	args db 10 dup ('$')	;przechowuje w kolejnych bitach dlugosci argumentow
	args_il db 3 dup ('$')	;przechowuje ilosc argumentow
	
	flaga_minus db 0
	flaga_kropka db 0
	
	zle_arg_1 db 	'Podano zly format argumentow.',13,10,
					'Prosze podac 4 liczby w formacie klasycznym',13,10,
					'np: -1.0 0.0 13.0 1.0','$',
					
					
	xRes dw 320
	yRes dw 200
	
	xmin dq -2.0
	xmax dq 1.0
	ymin dq -1.0
	ymax dq 1.0
	
	czesc_calkowita dw ?
	czesc_ulamkowa dw ?
	_10 dw 10d				;dla kooprocesora
	__10 db 10d				;dla mnozenia
	
	

	p							dq	?
	q							dq	?
	max							dq	4.0			; granica
	wordBuffer					dw	?			; CPU to FPU


	
data ends

code segment

parse proc
	
	pusha
	
	mov bx,80h
	mov ch,[bx] 				;wrzucam ilosc bitów (PYTANIE: CZY JEST ICH RAZEM ZE SPACJA CZY MNIEJ XD)
	
	mov bx,82h   				;do bx'a wrzucam adres do pierwszego znaku z lini komend
	
	mov ax,seg data				;zapisuje do argb ilosc bitow w psp
	mov es,ax
	mov di,offset argb
	

	mov byte ptr es:[di],ch
	
	mov di,offset argv 			;es:di - argv[0]

	mov cl,1					;w cl mam licznik bialego znaku, na początek ustawiam 1, to zagwarantuje, że pominie te spacje przed argumentami
	
	wypisuj:
		mov dl,[bx]				;pobieram znak z lini
		
		cmp ch,0  			;sprawdzam czy to już nie koniec argumentow
		je koniec				;jeżeli tak to koniec roboty
		dec ch					;zmniejszam ilosc arg do wczytywania
		
		
		cmp dl,' '				;sprawdzam czy znak jest spacją
		je zjedz_biale
		
		cmp dl,'	' 			;sprawdzam czy znak jest tabem
		je zjedz_biale
		
		xor cl,cl				;skoro wypisywanie tutaj doszło, to znak nie jest spacją/tabem, więc zeruję licznik spacji
		mov byte ptr es:[di],dl ;wpisuję znak do tablicy
		
		inc di					;przesuwam adres na kolejny element w argv
		inc bx					;przesuwam adres na kolejny znak w argumentach
		
		jmp wypisuj				 ;powtarzam aż znajdzie koniec lini
		
	zjedz_biale:
		cmp cl,0 				;jeżeli spacja jest pierwszą jaka się pojawiła
		je newline				;to daję nową linię
		inc bx					;w przeciwnym wypadku przesuwam adres na kolejny znak w argumentach
		jmp wypisuj				;oraz zaczynam wypisuwanie od nowa
		
	newline:					;wrzuca do tablicy znak nowej linii i powrót do początku Dh i Ah
		mov byte ptr es:[di],'!';po każdym argumencje daje znak '!'
		inc di		
		
		inc cl					;zwiększam licznik spacji, gdyż pierwsza się już pojawiła
		inc bx					;kolejny adres w psp
		
		jmp wypisuj 			;wracam do wypisywania
	
	koniec:
	dec di
		mov byte ptr es:[di],'!'
		popa
		ret
			
parse endp


wypisz_args proc
		pusha
		mov	ax,seg data
		mov	ds,ax
		mov dx,offset argv
		
		mov	ah,9  				; wypisz na ekran to co jest w ds:dx
		int	21h
		mov al,0
		popa
		ret
wypisz_args endp


policz_argumenty proc			;przechodzi przez argv, jest licznik cl który zlicza ilość znaków do '!', potem wpisuje do args, zeruje się i leci od nowa aż do znaku '$'
	pusha						;dodatkowo zlicza ich ilosc

	mov ax,seg data				;
	mov es,ax
	mov di,offset argv			;es:di - start argv	
	mov si,offset args			;es:si - start args
		
	xor cl,cl					;licznik dlugosci argumentow
	xor ch,ch					;licznik ilosci argumentow
	
	petla:
	mov dl,byte ptr es:[di]		;pobieram pierwszy znak z tablicy
	cmp dl,'$'
	je koniec_p
	
	cmp dl,'!'					;spra
	jne dodaj_licznik
	
	cmp dl,'!'
	je zapisz_dlugosc
	
	koniec_p:
	xor di,di
	mov di,offset args_il
	mov byte ptr es:[di], ch
	
	popa
	ret
	
	dodaj_licznik:
	inc cl
	inc di
	jmp petla
	
	zapisz_dlugosc:
	mov byte ptr es:[si],cl
	inc si
	inc di
	inc ch
	xor cl,cl
	jmp petla
	
policz_argumenty endp

sprawdz_argumenty proc
	pusha
	
	mov ax,seg data	
	mov es,ax
	
	sprawdz_czy_poprawne_4_arg:
		mov di,offset args_il	
		cmp byte ptr es:[di],4
		jne zla_ilosc
	
	xor ax,ax
	xor cx,cx						;w cl trzymam numer badanego znaku w ch-tym argumencie
									;w ch trzymam numer badanego argumentu
	
	mov di,offset argv
	sprawdz_czy_poprawny_format:
		cmp ch, 4					;przeszedlem przez wszystkie argumenty
		je okej						;wiec wszystkie poprawne
		
		cmp byte ptr es:[di],'!'			;sprawdzam koniec argumentu
		je nowy_argument
		
		cmp byte ptr es:[di],'-'			;sprawdzam czy znak -
		je ustaw_flage
		
		cmp byte ptr es:[di],'.'			;sprawdzam czy '.'
		je ustaw_kropke
		
		cmp byte ptr es:[di],'0'			;sprawdzam czy cyfra
		jb zla_ilosc
		
		cmp byte ptr es:[di],'9'			;sprawdzam czy cyfra
		ja zla_ilosc
		
		jmp next_arg
		
		
		
		cmp byte ptr es:[di],2		;sprawdzam ilość podanych argumentów
		jne zla_ilosc
		
	argumenty_ok:
		jmp okej
		
		
		
ustaw_kropke:
	cmp cl,0								;sprawdzam czy nie postawiono kropki na poczatku argumentu
	je zla_ilosc
	
	cmp byte ptr es:[di-1],'-'				;czy nie poprzedza kropki znak '-'
	je zla_ilosc
	
	cmp byte ptr es:[di+1],'!'				;czy po kropcie nie konczy sie argument
	je zla_ilosc
	
	mov si,offset flaga_kropka				;sprawdzam czy w badanej liczbie nie dano wiecej niz jedna kropka
	cmp byte ptr es:[si],0
	jne zla_ilosc
	
	inc byte ptr es:[si]					;jezeli przeszlo testy to ustawiam flage, ze istnieje juz kropka
	jmp next_arg
	
	
ustaw_flage:
	cmp cl,0								;sprawdzam czy '-' jest na pierwszym znaku argumentu
	jne zla_ilosc
	jmp next_arg
	
next_arg:
	inc cl							;inkrementuje zeby wiedziec ktory badam znak w argumencie
	inc di							;inkrementuje bo ide do kolejnego znaku w argv
	jmp sprawdz_czy_poprawny_format
	
nowy_argument:
	cmp cl,3						;sprawdzam czy argument mial conajmniej 3 znaki (musza byc bo np 1.0)
	jb zla_ilosc
	
	
	inc di							;nowy argument
	inc ch							;kolejny numer argumentu
	xor cl,cl						;zeruje cl ktory zawiera index znaku argumentu
	
	mov si,offset flaga_kropka		;sprawdzam czy w sprawdzonym argumencie wystapila kropka
	cmp byte ptr es:[si],0			
	je zla_ilosc
	
	mov byte ptr es:[si],0			;zeruje flage kropki
	jmp sprawdz_czy_poprawny_format
	

	
zla_ilosc:
	mov	ax,seg data
	mov	ds,ax
	mov dx,offset zle_arg_1
	jmp message
	
okej:
	jmp koniec_m	

message:
	mov	ah,9  				; wypisz na ekran to co jest w ds:dx
	int	21h
	mov al,0
	call Ender

koniec_m:
	popa
	ret
	
sprawdz_argumenty endp

przepisz_argumenty_do_zmiennych proc
pusha
	mov ax,seg data
	mov es,ax
	mov di,offset argv
	
	xor ax,ax
	xor cx,cx
	xor bx,bx
	xor dx,dx
	
	wybierz_zmienna:
	cmp bl,0	
	mov si,offset xmin
	je wczytaj_wartosc
	
	cmp bl,1	
	mov si,offset xmax
	je wczytaj_wartosc
	
	cmp bl,2	
	mov si,offset ymin
	je wczytaj_wartosc
	
	cmp bl,3	
	mov si,offset ymax
	je wczytaj_wartosc
	
	cmp bl,4
	je zakoncz_wczytywanie_do_zmiennych
	
	
	wczytaj_wartosc:
		cmp byte ptr es:[di],'-'
		je flaga_minusa
		
		ogarnij_inta:
			cmp byte ptr es:[di],'.'		;sprawdzam czy pod indexem di jest kropka
			je zapisz_czesc_calkowita
			
			mov dl,byte ptr es:[di]			;jezeli to nie ktopka to musi byc liczba 
			sub dl,'0'						;w takim razie odejmuje znak '0' by uzyskac z char'a int'a 
			
			mul byte ptr es:[__10]			;mnoze al przez 10
			add ax,dx						;dodaje do ax dx a w dx lezy liczba 
			
			inc di							;przechodze do indexu nowego znaku
			jmp ogarnij_inta				;wracam do poczatku petli wczytywania znaku
	
		
		zapisz_czesc_calkowita:
		push si								;w si trzymam offset zmiennej do ktorej wczytuje liczbe rzeczywista wiec nie moge uzyc czyli rzucam na stos
		mov si,offset czesc_calkowita		;i wczytuje offset zmiennej przechowujacej liczbe calkowita
		mov word ptr es:[si],ax				;czesc calkowita jest slowem, w ax siedzi czesc calkowita wiec wrzucam ja do zmiennej
		pop si								;przywracam ze stosu offset zmiennej do ktorej wczytam liczbe rzeczywista
		inc di								;przechodze do kolejnego argumentu poniewaz stoje na kropce
		xor ax,ax 							;zeruje ax'a bo bede otrzymywal nowa liczbe
		
		ogarnij_float:						;przeszedlem przez czesc calkowita, ustawilem index na pierwsza liczbe po kropce wiec zaczynam czesc ulamkowa
			cmp byte ptr es:[di],'!'		;znak '!' konczy kazdy argument czyli oznacza koniec czesci ulamkowej
			je zapisz_czesc_ulamkowa		;oznacza to ze musze wczytac liczbe ulamkowa
			
			mov dl,byte ptr es:[di]			;przenosze liczbe z czesci ulamkowej do dl'a
			sub dl,'0'						;ponownie odejmuje znak '0' by otrzymac liczbe zamiast znaku ascii
			
			mul byte ptr es:[__10]			;mnoze to co w ax przez 10 
			add ax,dx						;i dodaje kolejna liczbe
			
			inc di							;i przechodze do kolejnego indexu w tablicy argumentow
			inc cx							;zwiekszam licznik cx ktory powie mi potem ile razy ma kooprocesor ma podzielic liczbe z czesci ulamkowej by z calkowitej otrzymac prawdziwie ulamkowa np z 123 -> 0.123
			jmp ogarnij_float				;wykonuje funkcje ponownie az napotkam '!'
			
		zapisz_czesc_ulamkowa:
		push si								;w si trzymam offset zmiennej do ktorej wczytuje liczbe rzeczywista wiec nie moge uzyc czyli rzucam na stos
		mov si,offset czesc_ulamkowa		;i wczytuje offset zmiennej przechowujacej liczbe ulamkowa (w postaci calkowitej)
		mov word ptr es:[si],ax				;czesc ulamkowa jest slowem, w ax siedzi czesc ulamkowa (w postaci calkowitej) wiec wrzucam ja do zmiennej
		pop si								;przywracam ze stosu offset zmiennej do ktorej wczytam liczbe rzeczywista
		
		
		
		wrzuc_liczby_do_kooprocesora_i_zamien_na_floata:		;przeszedlem przez wczytywanie czesci calkowitej i ulamkowej wiec teraz bede zamieniac na rzeczywista
		fild word ptr es:[czesc_calkowita]			; st(2) 	- pobieram do kooprosesora czesc calkowita
		fild word ptr es:[_10]						; st(1) 	- pobieram liczbe 10 ktorej uzyje do dzielenia czesci ulamkowej
		fild word ptr es:[czesc_ulamkowa]			; st(0) 	- pobieram do kooprocesora czesc ulamkowa (w postaci calkowitej
		
			tofloatloop:							;funkcja zamienia czesc ulamkowa (w postaci calkowitej) do prawdziwie ulamkowej	
				fdiv st(0), st(1)					;dlatego st(0) = st(0)/st(1)
				loop tofloatloop					;wykonuje tyle razu ile cyfr miala czesc ulamkowa - tyle ile zinkrementowalem w cx
				
				fadd st(0), st(2)					;po skonczeniu dodaje czesc calkowita z ulamkowast(0) = st(0)+st(2)
		
				cmp byte ptr es:[flaga_minus], 1d	;sprawdzam czy liczba posiadala znak '-' (czy jest ujemna)
				jne zapisz_liczbe_zmiennoprzecinkowa ;jezeli nie byla to zapisuje powstala liczbe do odpowiadajacej zmiennej
				fchs						; number*=-1
		
		zapisz_liczbe_zmiennoprzecinkowa:
		fstp qword ptr es:[si]						;wrzucam do zmiennej ktora offset ma w si wartosc z st(0) i wyrzucam ja z kooprocesora 
		
		fsubp st(0), st(0)							;zwalniam kooprocesor z liczb zalegajacych
		fsubp st(0), st(0)			
		
		jmp do_nowego_argumentu						;wykonalem obliczenia dla argumentu, przepisalem go w prawidlowej formie do zmiennej wiec ide do kolejnej
		
	
	
flaga_minusa:
	mov byte ptr es:[flaga_minus],1
	inc di
	jmp wczytaj_wartosc

do_nowego_argumentu:
	inc di											;wchodze na poczatek nowego argumentu
	inc bl											;kolejny numer argumentu w wyborze do wpisywania
	mov byte ptr es:[flaga_minus],0					;zeruje flage ujemnego znaku
	jmp wybierz_zmienna								;skacze do miejsca w ktorym chce wczytaj kolejna zmienna
	
	
	zakoncz_wczytywanie_do_zmiennych:
popa
ret
przepisz_argumenty_do_zmiennych endp



rysuj_mandelbrota proc 					; calculates points, draws mandelbrot set in video mode
	mov ax,seg data
	mov es,ax
	
	call videomode
	call rysuj
	call czekaj_na_klawisz 			
	call wyjdz_z_videomode
ret
rysuj_mandelbrota endp

videomode proc					; change to video mode

	push ax
	xor ah, ah
	mov al, 0dh						
	int 10h
	pop ax
ret
videomode endp

rysuj proc 							; draws mandelbrot set based on calculated p, q and pixel value

	pusha
	pushf
	
	xor cx, cx						; set coordinates to (0,0)
	xor dx, dx
	
	petla_po_Y:
		cmp dx, 200d 				; if dx at end of column
		JE zakoncz_rysowanie
		petla_po_X:
			cmp cx, 320d			; if cx at end of row
			JE koniec_wiersza				
			mov ax, cx
			call wylicz_p
			mov ax, dx
			call wylicz_q
			call wylicz_pixel
			cmp al, 1d
			JB czarny_pixel				; if AL==0 draw black pixel
			mov al, 0Fh				; set pixel color to white
			JMP rysuj_pixel
		czarny_pixel:
			xor al, al				; set pixel color to black
		rysuj_pixel:
			mov ah, 0ch
			int 10h
			inc cx
			JMP petla_po_X				; next pixel in row
		koniec_wiersza:
			xor cx, cx				; back to first pixel in row
			inc dx					; dx+=1, go to next column
	JMP petla_po_Y
	
	zakoncz_rysowanie:
	popf
	popa
ret
rysuj endp

wylicz_p: 						; input - in ax, coordinate, saves calculated value to memory
		
	mov word ptr es:wordBuffer, ax		; write from ax to buffer, can't move directly from CPU to FPU
	fld qword ptr es:[xmax]			; st(4) - xmax
	fld qword ptr es:[xmin]			; st(3) - xmin
	fild word ptr es:[xRes]		; st(2) - horizontal resolution
	fild word ptr es:[wordBuffer]		; st(1) - coordinate
	fldz							; st(0) - p=0
	
	fadd st(0), st(4)				; p = xmax
	fsub st(0), st(3)				; p = xmax - xmin
	fmul st(0), st(1)				; p = N*(xmax - xmin)
	fdiv st(0), st(2)				; p = N*(xmax - xmin)/xResolution
	fadd st(0), st(3)				; p = xmin + N*(xmax - xmin)/xResolution
	
	fstp qword ptr es:[p]				; write p to memory and pop

	fsubp st(0), st(0)				; free coprocessor stack
	fsubp st(0), st(0)
	fsubp st(0), st(0)
	fsubp st(0), st(0) 				; coprocessor stack is empty
ret

wylicz_q: 						; input - in ax, coordinate, saves calculated value to memory
	
	mov word ptr es:wordBuffer, ax		; write from ax to buffer, can't move directly from CPU to FPU
	fld qword ptr es:[ymax]			; st(4) - ymax
	fld qword ptr es:[ymin]			; st(3) - ymin
	fild word ptr es:[yRes]		; st(2) - vertical resolution
	fild word ptr es:[wordBuffer]		; st(1) - coordinate
	fldz							; st(0) - q=0
	
	fadd st(0), st(4)				; q = ymax
	fsub st(0), st(3)				; q = ymax - xmin
	fmul st(0), st(1)				; q = N*(ymax - ymin)
	fdiv st(0), st(2)				; q = N*(ymax - ymin)/yResolution
	fadd st(0), st(3)				; q = ymin + N*(ymax - ymin)/yResolution
	
	fstp qword ptr es:[q]				; write q to memory and pop

	fsubp st(0), st(0)				; free coprocessor stack, could use fstp st(0)
	fsubp st(0), st(0)
	fsubp st(0), st(0)
	fsubp st(0), st(0)				; coprocessor stack is empty
ret

wylicz_pixel: 					; out - result in al

	push cx
	pushf 							; !

	fld qword ptr es:[p]				; st(7) = p
	fld qword ptr es:[q]				; st(6) = q
	fldz							; st(5) = 0 - for tmp
	fldz							; st(4) = 0 - for x
	fldz							; st(3) = 0 - for x*x
	fldz							; st(2) = 0 - for y
	fldz							; st(1) = 0 - for y*y
	fldz							; st(0) = 0 - for additional tmp variable
	
	mov cx, 1000d					; set loop counter for 1000 iterations
	
	dokonaj_obliczen:
	; tmp
	fsub st(0), st(0)				; st(0) = 0
	fadd st(0), st(3)				; st(0) = x*x
	fsub st(0), st(1)				; st(0) = x*x-y*y
	fadd st(0), st(7)				; st(0) = x*x-y*y+p
	fxch st(5)						; tmp = st(5) = x*x-y*y+p
	; y
	fsub st(0), st(0)				; st(0) = 0
	fadd st(0), st(4)				; st(0) = x
	fmul st(0), st(2)				; st(0) = x*y
	fadd st(0), st(0)				; st(0) = 2*x*y
	fadd st(0), st(6)				; st(0) = 2*x*y+q
	fxch st(2)						; st(2) = y =2*x*y+q
	; x
	fsub st(0), st(0)				; st(0) = 0
	fadd st(0), st(5)				; st(0) = tmp
	fxch st(4)						; st(4) = x = tmp
	; x*x + y*y
	fsub st(0), st(0)
	fadd st(0), st(4)				; st(0) = x
	fmul st(0), st(0)				; st(0) = x*x
	fxch st(3)						; st(3) = x*x
	fsub st(0), st(0)				; st(0) = 0
	fadd st(0), st(2)				; st(0) = y
	fmul st(0), st(0)				; st(0) = y*y
	fxch st(1)						; st(1) = y*y
	fsub st(0), st(0)				; st(0) = 0
	fadd st(0), st(3)				; st(0) = x*x
	fadd st(0), st(1)				; st(0) = x*x+y*y
	; check exit condition
	fcom es:[max]
	fstsw ax						; tricky part - fcom saves flags to status word (16 bit), fstsw ax stores it in ax
	sahf							; from ah to flag register - in status word 14th and 8th bits stand for ZF and CF
	JA break_					; if st(0) > max break loop - JA checks carry flag (CF) and ZF
	loop dokonaj_obliczen				
	
	mov al, 1d						; all iterations performed
	JMP koncz_prace_na_kooprocesorze

	break_:
	mov al, 0d						; <1000 iterations performed
	
	koncz_prace_na_kooprocesorze:
	fsubp st(0), st(0) 				; free coprocessor stack
	fsubp st(0), st(0)
	fsubp st(0), st(0)
	fsubp st(0), st(0)
	fsubp st(0), st(0)
    fsubp st(0), st(0)
	fsubp st(0), st(0)
	fsubp st(0), st(0)

	popf
	pop cx
ret

czekaj_na_klawisz proc					; waits for keystroke

	push ax
	xor ax, ax
	int 16h
	pop ax
ret
czekaj_na_klawisz endp

wyjdz_z_videomode proc 						; back from video mode
	push ax
	xor ah, ah
	mov al, 3d						
	int 10h
	pop ax
ret
wyjdz_z_videomode endp

Ender proc					;konczy program
	mov ah,4ch
	int 21h
	ret
Ender endp



start:
call parse
call policz_argumenty
call sprawdz_argumenty
call przepisz_argumenty_do_zmiennych
call rysuj_mandelbrota
call Ender

	
code ends


stos1	segment stack
		dw	200 dup(?)
		db 200 dup(?)
top1	dw	?
stos1	ends
end start

