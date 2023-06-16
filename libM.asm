;DIAZ MATIAS HERNAN
;LIBRERIA
.8086
.model small
.stack 100h

.data
	datamul db 1,10,100
	databin db 1,2,4,8,16,32,64,128
	salto 	db 0dh,0ah,24h
	msj1 	db "Error en la carga vuelva a intentar",0dh,0ah,24h
.code

public imprimir
public mayusculizar
public minimizador
public regToBin
public asciiToReg
public regToAscii
public regToBinAscii
public cargaTexto
public buscaYCuenta
public reemplazarCaracter
public cargaNum
public cargaSoloBin
public posChar
public binToReg

	proc imprimir

		;RECIBE POR STACK EL OFFSET DEL TEXTO A IMPRIMIR
		
		push bp
		mov bp,sp 

		push ax
		push bx
		push cx
		push dx
		pushf

		mov bx,ss:[bp+4] ;offset del texto

		mov ah,9
		mov dx,bx
		int 21h

		popf
		pop dx
		pop cx
		pop dx
		pop ax
		pop bp

		ret 2

	imprimir endp

;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

	proc mayusculizar

		;RECIBE POR STACK EL OFFSET DEL TEXTO A MAYUSCULIZAR

		push bp
		mov bp,sp 

		push ax
		push bx
		push cx
		push dx
		push si
		pushf

		mov bx,ss:[bp+4] ; offset del texto

		
proceso_mayuscula:
		cmp byte ptr [bx],24h
	je fin_mayuscula
		cmp byte ptr [bx],96 ; es menor que a
	jbe incremeta_bx
		cmp byte ptr [bx],123 ; es mayor que z
	jae incremeta_bx
		sub byte ptr [bx],20h	
incremeta_bx:
		inc bx
	jmp proceso_mayuscula
fin_mayuscula:

		popf
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop bp

		ret 2

	mayusculizar endp

;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------


	minimizador proc

	;RECIBE POR STACK EL OFFSET DEL TEXTO A MINIMIZAR

		push bp
		mov bp,sp 

		push ax
		push bx
		push cx
		push dx
		push si
		pushf

		mov bx,ss:[bp+4] ; offset del texto

		
proceso_minuscula:
		cmp byte ptr [bx],24h
	je fin_mayuscula
		cmp byte ptr [bx],64 ; es menor que a
	jbe incremeta_bx2
		cmp byte ptr [bx],90 ; es mayor que z
	jae incremeta_bx2
		add byte ptr [bx],20h	
incremeta_bx2:
		inc bx
	jmp proceso_minuscula
fin_minuscula:

		popf
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop bp

		ret 2


	minimizador endp

;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

	regToBin proc
	;RECIBE POR REGISTRO EN BL** UN NUMERO MENOR A 256 Y LO IMPRIME!
	
		push ax
		push bx
		push cx
		pushf

		mov cx, 8
shifteo:
		shl bl,1
	jc esUno
		mov ah,2
		mov dl,30h
		int 21h
incremento:
	loop shifteo
	jmp fin

esUno:
		mov ah,2
		mov dl,31h
		int 21h
	jmp incremento

fin:
		popf
		pop cx
		pop bx
		pop ax

		ret

	regToBin endp


;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

regToBinAscii proc
	;GUARDA EN UNA CADENA EL BINARIO CORRESPONDIENTE A UN NUMERO
	;RECIBE POR AL PRIMERO** OFFSET DE UNA CANTIDAD 
	;RECIBE POR STACK SEGUNDO** OFFSET DE UNA CADENA PARA GUARDA EL BINARIO ASCII QUE REPRESENTA
	; "00000000"

		push bp
		mov bp,sp 

	;GUARDADO DEL ENTORNO
		push ax
		push bx
		push cx

		mov bx, ss:[bp+4] ; offset de la cadena de "00000000"
		mov cx, 8 
arriba:
		shl al, 1 
	jc esUno1

	continua:
		inc bx
	loop arriba
	jmp fin1

	esUno1:
		mov byte ptr[bx], 31h
	jmp continua

fin1:
		pop cx
		pop bx
		pop ax
		pop bp
		ret 2

	regToBinAscii endp


;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

	regToAscii proc
	;RECIBE POR STACK - PRIMERO** OFFSET VARIABLE "000" ASCII
	;RECIBE POR STACK - SEGUNDA** OFFSET DEL VALOR EN DECIMAL
	;MODIFICA EL "000" AL VALOR DE LA VARIABLE EN DECIMAL
	
		push bp
		mov bp, sp

		push ax
		push bx
		push cx
		push dx
		push si
		pushf
			
		xor ax,ax

		mov bx, ss:[bp+6] ; offset del texto
		add bx, 2         
		mov si, ss:[bp+4] ;offset del numero
		mov al, byte ptr[si] 
		mov dl, 10
		mov cx, 3
		
convierte:
		div dl
		add ah, 30h
		mov byte ptr[bx], ah 
		xor ah, ah
		dec bx 
	loop convierte

		popf
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop bp

		ret 4 

	regToAscii endp

;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

	asciiToReg proc
	;RECIBE POR REGISTRO BX UN OFFSET QUE CORRESPONDE A UN NRO
	;DE 3 DIGITOS ASCII, Y DEVUELVE EN DH** EL NRO CONVERTIDO

		push ax
		push bx
		push si
		push cx
		pushf

		lea si, datamul
		add bx, 2 
		mov cx, 3
		xor dx, dx

convertir:
		mov al, byte ptr[bx]
		sub al, 30h
		mov dl, [si]
		mul dl
		add dh, al 
		inc si
		dec bx
	loop convertir

		popf
		pop cx
		pop si
		pop bx
		pop ax

		ret

	asciiToReg endp


;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------
	binToReg proc 
		;RECIBE POR REGISTRO BX LA CADENA BINARIA EJ "10101011"
		;GUARDA EN DH EL NUMERO EN DECIMA

		push bp
		mov bp,sp 
		push ax
		push si 
		pushf

		lea si, databin
		add bx, 7 
		mov cx, 8
		xor dx, dx

convertir1:
		mov al, byte ptr[bx]
		sub al, 30h
		mov dl, [si]
		mul dl
		add dh, al 
		inc si
		dec bx
	loop convertir1


		popf
		pop si
		pop ax
		pop bp

		ret 

	binToReg endp
;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

	cargaTexto proc

	;RECIBE LA DIRECCION POR STACK DE LA VARIABLE TEXTO A COMPLETAR
	;RECIBE POR REGISTRO:
	;DH -> CARACTER DE FINALIZACION, POR DEFECTO 0 -> FINALIZO ODH
	;DL -> 0 LEO HASTA 256, 1 -> HASTA EL NUMERO INDICADO

		push bp
		mov bp,sp

		push ax
		push bx
		push cx
		push dx 
		pushf

		mov bx,ss:[bp+4] ;offset del texto

	;COMPRUEBO SOLO SI TIENE O NO CONDICION DE SALIDA

		cmp dh,0
	je esEnter
	jmp continuaCarga

esEnter:
		mov dh,0dh

continuaCarga:

	;CARGO TEXTO Y VEO SI ES FIJA LA CANTIDAD O ILIMITADA
		cmp dl,0
	jne cargaCantidad

	;CARGA ILIMITADA CON CARACTER DE SALIDA
cargaTexto1:
		;cmp bx,256
		;je finCarga
		mov ah, 1
		int 21h
		cmp al, dh
		je finCarga
		mov byte ptr[bx], al
		inc bx
	jmp cargaTexto1 


	;CARGA CON CANTIDAD FIJA (DL != 0)
cargaCantidad:
		xor cx, cx 
		mov cl, dl
 leeCantidad:
		mov ah, 1
		int 21h
		mov byte ptr[bx], al
		inc bx
	loop leeCantidad

finCarga:

		popf
		pop dx
		pop bx
		pop cx 
		pop ax
		pop bp

		ret 2

	cargaTexto endp

;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------
	cargaNum proc 

	;CARGA EXCLUSIVAMENTE NUMEROS
	;RECIBE UNA VARIABLE TEXTO DE LA FORMA "00000" O "000" PARA 3 DIG
	;RECIBE POR DL = 0 CARGA 3 MUMEROS, DL = 1 CARGA 5
	;EN DL = 0 NO PERMITE UNA CARGA DE NUMERO MAYOR A 256
	;SI INGRESO 123 SALE 00123
	;SI CL = 1 REEMPLAZA LOS 0 POR 20H PARA QUE NO SE VEAN AL IMPRIMIR

		push bp
		mov bp, sp

		push ax
		push bx
		push cx

		mov bx, ss:[bp+4] ;offset de la variable texto

		cmp dl, 0
		je son3
		mov cx, 5

cargaNro5:
		mov ah, 1
		int 21h
		mov [bx],al 
		inc bx
	loop cargaNro5
jmp finCarga1
son3:
	add bx,0
cargaNro3:
		mov ah, 1
		int 21h       ;LEO EL 1er NRO
		cmp al, 32h   ;SI ES MENOR O IGUAL A 2 GUARDO
	jbe segundoNro
	jmp cargaNro3 ;RESETEO
segundoNro:
		mov byte ptr [bx],al   ;GUARDO EL 1er NRO
		mov ah, 1     ;LEO EL 2do
		int 21h
		cmp byte ptr[bx], 32h ; SI el 1ro es MENOR a 2 no hay limite 
	jne PuedeSer9
		cmp al, 35h   ;SINO COMO máXIMO DEBE SER 5
	jbe tercerNro    ;SI CUMPLE CARGA EL 3ro
	jmp cargaNro3
PuedeSer9:
		cmp al, 39h   ; SI EL 1ro era menor a 2 puedo cargar 9
	jbe tercerNro
	jmp cargaNro3

tercerNro:
		inc bx         ;ALMACENO EL 2do NRO
		mov byte ptr [bx],al
		mov ah, 1      ;CARGO EL 3ro
		int 21h
		cmp byte ptr [bx-1], 32h ; SI EL PRIMERO ES IGUAL a 2
	jne PuedeSer92
		cmp byte ptr [bx], 35h   ;Y EL 2do IGUAL A 5
	jne PuedeSer92  
		cmp al, 35h     ;EL 3ro nO PUEDE SER MAYOR A 5
	jbe Completo
	jmp cargaNro3
PuedeSer92:
		cmp al, 39h    ;SINO PUEDE SER MENOR O IGUAL A 9
	jbe Completo
	jmp cargaNro3
Completo:
		inc bx
		mov byte ptr[bx],al    ;GUARDO EL 3er NRO

; CL = 1 BORRA LOS CEROS

		cmp cl,0
	je finCarga1
		mov al,20h
		mov bx,0
recorrer3:
		cmp bx,24h
	je finCarga1
		cmp byte ptr [bx],"0"
	je ponerEspacio
decrementar_bx:
		inc bx
	jmp recorrer3
ponerEspacio:
		mov byte ptr [bx],al
	jmp decrementar_bx

finCarga1:

		pop cx
		pop bx
		pop ax
		pop bp
		ret 2

	cargaNum endp
;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

	cargaSoloBin proc 

		;SOLO PERMITE LA CARGA DE 0s Y 1s HASTA 8 DIGITOS
		;RECIBE EL OFFSET POR STACK DE LA VARIABLE TEXTO 
		;DE LA FORMA "00000000"

		push bp 
		mov bp,sp 
		push ax
		push bx
		push cx
		pushf

		mov bx,ss:[bp+4] ; offset de la variable texto

		mov cx,8
		jmp loopcarga
nuevaCarga:
		mov ah,9
		lea dx,salto
		int 21h

		mov ah,9
		lea dx,msj1
		int 21h

		mov ah,9
		lea dx,salto
		int 21h

		mov bx,ss:[bp+4] ; offset de la variable texto

		mov cx,8
loopcarga:
		mov ah,1
		int 21h
		cmp al,30h
	je esUn1o0
		cmp al,31h
	je esUn1o0
		jmp nuevaCarga
incremeta_bx4:
		inc bx
	loop loopcarga

	jmp finCargaBin

esUn1o0:
		mov byte ptr [bx],al
	jmp incremeta_bx4


finCargaBin:

		popf
		pop cx
		pop bx
		pop ax
		pop bp

		ret 2


	cargaSoloBin endp

;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

	buscaYCuenta proc

	;CUENTA LA CANTIDAD DE VECES QUE SE REPITE UN CARACTER EN UNA VARIABLE
	;POR AX EL OFFSET DEL TEXTO
	;POR DX EL CARACTER A CONTAR
	;SE ALMACENA LA CANTIDAD EN CX

		push bp
		mov bp,sp 
		push ax
		push dx
		pushf
		
		mov bx,ss:[bp+4]	;offset del texto
		xor cx,cx 
recorrer:
		cmp byte ptr [bx],24h
	je fin2
		cmp byte ptr [bx], dl	;caracter a contar
	je cont
incremeta_bx3:
		inc bx
	jmp recorrer
cont:
		inc cx				
	jmp incremeta_bx3
fin2:
		popf
		pop dx
		pop ax
		pop bp
		
		ret 2

	buscaYCuenta endp

;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

	reemplazarCaracter proc
	;RECIBE PRIMERO** EL OFFSET DEL TEXTO A RECORRER
	;RECIBE SEGUNDO** EL OFFSET DEL TEXTO A LLENAR CON EL CARACTER NUEVO
	;RECIBE POR DL EL CARACTER A REEMPLAZAR
	;RECIBE POR DH EL CARACTER NUEVO

		push bp
		mov bp,sp 

		push ax
		push bx 
		push cx 
		push dx
		pushf

		mov bx,ss:[bp+4]; offset del texto vacio
		mov si,ss:[bp+6]; offset del texto a recorrer
		mov al,0dh
recorrer2:
		cmp byte ptr [si],dl
	je reemplazar
		cmp byte ptr [si],al
	je fin3
		mov ah, byte ptr [si]
		mov byte ptr [bx], ah
incremeta:
		inc bx
		inc si 
	jmp recorrer2

reemplazar:
		mov byte ptr [bx],dh 
		jmp incremeta

fin3:
	popf
	pop dx
	pop cx 
	pop bx 
	pop ax
	pop bp

	ret 4

	reemplazarCaracter endp


;-----------------------------------------------------------------------
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;-----------------------------------------------------------------------

	posChar proc
		;RECIBE PRIMERO** POR STACK EL OFFSET DE UNA CADENA DE TEXTO
		;POR REGISTRO SEGUNDO** DL EL CARACTER BUSCADO
		;CX POSICION EN LA CADENA
		;SI NO ENCUENTRA EL CARACTER DEVUELVE -1 -> 255

		push bp
		mov bp,sp 
		push ax
		push bx
		push dx 
		pushf
		xor cx,cx
		mov bx,ss:[bp+4] ;offset de la cadena
		mov si,0
recorrer4:
		cmp byte ptr [bx],24h
	je no_Encontro
		cmp byte ptr [bx],dl 
	je posicion
		inc bx
		inc si 
	jmp recorrer4

no_Encontro:
		mov cx,-1
	jmp fin_recorrer

posicion:
		mov cx,si
fin_recorrer:

		popf
		pop dx 
		pop bx
		pop ax 
		pop bp

		ret 2

	posChar endp


end