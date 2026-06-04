<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:math="urn:schemas-cagle-com:math" xmlns:msxsl="urn:schemas-microsoft-com:xslt"
    xmlns:script="Myscript">
<!--    <xsl:output version="4.0" omit-xml-declaration="yes" indent="yes" method="text" />-->
    <xsl:output method="xml" encoding="UTF-8" indent="yes"/>
    <!-- _______________________________________________________ Root
    ____________________________________________________________ -->
    <msxsl:script language="VBScript" implements-prefix="script"><![CDATA[ 

       	dim i
       	dim j
       	dim k
		dim LenCad
		dim PN 
		
		i = 0
		j = 0
		k=10000
		PN =0
		LenCad=0
		
		Dim currentLocale
		' Get the current locale
		currentLocale = GetLocale
		
		Function Inc()
		
			i = i+1
			Inc = i
			
		End Function 
		
		Function IncKTN()
		
			j = j+1
			IncKTN = j
			
		End Function 
		
		Function IncPN()
		
			PN =PN+1
			IncPN = PN
			
		End Function 
		
		Function  ResetPN()
		
			PN = 0
			ResetPN =""

		End Function		
		
			Function Dec()
			
			k=k+1
			Dec=k
			
		End Function

		Function  Reset()
		
			i = 0
			Reset =""

		End Function
				
		Function GetPosition(angle)
		
			Select Case angle
				Case "0", "360": GetPosition= "00"
            		Case "90": GetPosition= "01"           
        			Case "180": GetPosition= "02"           
        			Case "270": GetPosition= "03"            
    			End Select
    		End Function
    		
    		
    	     Function GetElementNo(role, ang, fc)
                Dim a
                Dim e
                GetElementNo = "000"
                If IsNumeric(ang) Then a = CInt(ang) Else Exit Function
                e = 0
                Select Case role
                    Case "FRAME": e = 100
                    Case "SASH": e = 1
                    Case "MULLION"
                        If fc = "" Then
                            If a = 90 Or a = 270 Then e = 200
                            If a = 0 Or a = 360 Or a = 180 Then e = 300
                        Else
                            If a = 90 Or a = 270 Then e = 700
                            If a = 0 Or a = 360 Or a = 180 Then e = 800
                        End If
                    Case "SASH STOP": e = 400
                End Select
                GetElementNo = Right("000" & CStr(e), 3)
            End Function
    		
    		            

    		
    		
	Function GetSituation(angle, role)	
			Dim a 
			a = angle + 0
			
			Select Case TRUE
        			Case a >= 0 and a < 90: 
  			      	 	If role="MULLION" or role="SASH STOP" Then
        					GetSituation= "WA"        
            	        Elseif role = "FRAME" then
            	        	GetSituation="UN"
            	        elseif role = "SASH"then
            	         	GetSituation="UN"        					   
        				Else
		  			       GetSituation= "WA"      
						End If

            	    Case a >= 90 and a < 180: 
				If role="MULLION" or role="SASH STOP" Then
            	        	GetSituation= "SE"
            	        Elseif role = "FRAME" then
            	        	GetSituation="RE"
            	        elseif role = "SASH"then
            	         	GetSituation="RE"
            	        Else
            	         	GetSituation="SE"
            	        End If           
        			Case a >= 180 and a < 270: 
        				If role="MULLION" or role="SASH STOP" Then
        					GetSituation= "WA"      
            	        Elseif role = "FRAME" then
            	        	GetSituation="OB"
            	        elseif role = "SASH"then
            	         	GetSituation="OB"        					     
        				Else
        					GetSituation="WA"
        				End If
        			Case a >= 270 and a < 360: 
        				If role="MULLION" or role="SASH STOP" Then
        					GetSituation= "SE"
            	        Elseif role = "FRAME" then
            	        	GetSituation="LI"
            	        elseif role = "SASH"then
            	         	GetSituation="LI"        					
        				Else
        					GetSituation="SE"
        				End If
  			       Case a = 360: 
  			      	 	If role="MULLION" or role="SASH STOP" Then
        					GetSituation= "WA"        
            	        Elseif role = "FRAME" then
            	        	GetSituation="UN"
            	        elseif role = "SASH"then
            	         	GetSituation="UN"
        					   
        				Else
		  			       GetSituation= "WA"      
						End If
		  			Case Else: GetSituation="WA"
    			End Select
 
		End Function    		
    		
    					
		Function Derecha(cad1, Lt, sz)
    			
    		Dim S
			Dim L
    			    			    			
    		L = Len(cad1)
    		If L > Lt Then
        		Derecha = Left(cad1, Lt)
    		Else 
    			If sz="z" Then   				
	        		S = string(Lt - L, "0")       	 			
				Else
					S=string(Lt-L," ")
				End If
				Derecha=cad1 & S
     		End If
    			
		End Function
		
		Function Izquierda(cad1, Lt, sz)
    			
    		Dim S
    		Dim L
    			    			    			
    		L = Len(cad1)
    		If L > Lt Then
        		Izquierda = Left(cad1, Lt)
    		Else 
    			If sz="z" Then   				
	        		S = string(Lt - L, "0")       	 			
				Else
					S=string(Lt-L," ")
				End If
				Izquierda=S & cad1 
   			End If
    			
		End Function

		Public Function Redondea(d, n) 
    			
			Dim k 
    			
        	k = 10^n
    		Redondea = CLng(d * k) / k
		
		End Function

		Public Function LongField(n, w) 
    			
    		Dim S 
    		Dim l 
    
    		S = Cstr(n)
    		S = Trim(S)
    		l = Len(S)
    		For n = 1 To w - l
        		S = "0" + S
    		Next
    		LongField = S

		End Function
		
		Public Function DoubleField( d , w) 
    			
    		Dim n
		    n = Redondea(d, 1) * 10
			DoubleField = LongField(n, w)
		
		End Function	
		
		Public Function GetAngles(AA, AB, inverted)
			
			Dim dAA, dAB
			
			If inverted="1" Then
				dAA=CInt(AB)
				dAB=CInt(AA)
			Else
				dAA=CInt(AA)
				dAB=CInt(AB)
			End If
    
    		GetAngles = LongField (dAA * 10, 4) &  LongField (dAB * 10, 4)
    		
    	End Function
    		
	  Public Function Tools(OperationsList, inverted, length,weldA,weldB)
		
			dim objOp,objOp2,objOpp
			dim n, m,j,f,intContUltOpVal,i
			dim dValue
			dim strName,strNamee,strDescription,strName2
			dim strTool
			dim dValor1 
			dim dValorZ
			dim dValorY
			dim Z
			dim Y
			dim varList
			dim varNode
			dim dValueY
			
			dim HorizontalInversion
										
			Tools=""
							
			n=OperationsList.length
				
			'contamos las operaciones realmente Validas >> las primitivas	
			For j=0 to n-1
				set objOpp=OperationsList.Item(j)
				strNamee = objOpp.getAttribute("nameInMachine")
				if strNamee = "" then
					strNamee=objOpp.getAttribute("name")
				end if
				
				'If Left(strNamee,1)="W" Then
				If Left(strNamee,1)="W" and Len(strNamee)=8 Then
					intContUltOpVal=j
				End If
			Next
			
			If n=0 Then    				
				Exit Function
			End If

				
			For m=0 to n-1
				'inicializamos dValorZ a cero
				dValorZ=0
				dValorY=0
				set objOp=OperationsList.Item(m)
				strName= objOp.getAttribute("nameInMachine")
				if strName = "" then
					strName=objOp.getAttribute("name")
				end if
				HorizontalInversion = objOpp.getAttribute("HorizontalInversion")
				
				
				strDescription=objOp.getAttribute("description")
				'If Left(strName,1)="W" Then
				If Left(strName,1)="W" and Len(strName)=8 Then
				
					set varList = objOp.childNodes
					i=varList.length
					
					For j=0 to i-1
						set varNode=varList.Item(j)
						'dValorZ = 0
						'dValorY = 0
						If not varNode is Nothing Then
							'If varNode.getAttribute("name")= "Lbbbbb" Then
							'	dValorZ = varNode.getAttribute("value")
							'	exit for						
							'End If
							If varNode.getAttribute("name")= "Lbbbbb" or varNode.getAttribute("name")= "Milling" Then
								dValorZ = varNode.getAttribute("value")
							Else
								'If varNode.getAttribute("name")= "Lyyyyy" Then
								'	dValorY = varNode.getAttribute("value")					
								'End If
								'y for VNotch operations
								If varNode.getAttribute("name")= "MW" Then
									dValorY = varNode.getAttribute("value")/2		
								End If
							End if							
						End If
					next					
					
					If strName<>"H2150001" and strName<>"H2150002" Then
						'if inverted="1" then
						'	dValor1=objOp.getAttribute("X") + (weldB)
						'else
						'	dValor1=objOp.getAttribute("X") + (weldA)
						'End If
						'modifica javi, perque les ooperacions aÃ§i estan ordenades en sentit prefcad, aixi que es deuen modificar en sentit prefcad
						dValor1=objOp.getAttribute("X") + (weldA)
					else
						'porque H2150001 y H2150002 ya se ponene correctamente por VBA
						dValor1=objOp.getAttribute("X") 
					End If
					
					
					IF strName ="H1130099" then
						if dValor1 < (length/2) then
							dValor1 = 0
						else
							dValor1 = length
						end if
					end if
					Z=DoubleField(dvalorZ,5)
					'Y=DoubleField(dvalorY,4)
					'modificamos el 15-12-05
					Y=DoubleField(dvalorY,5)
					
					'If dValorZ > 0 Then
					'aÃ±ade javi el 14-12-05 par YKK
					'H0290001 V-Notch
					'If dValorZ > 0  and strName<>"H0290001"  Then
					'vuelo a dejar igual el 14 a la noche
					If dValorZ > 0  Then
						'If inverted=1 or HorizontalInversion = 1 then
						'paso de invertes
						If  HorizontalInversion = 1 then
							dValue=DoubleField((length - dValor1)-dValorZ/2,5)
						Else					
							dValue=DoubleField(dValor1-dValorZ/2,5)
						End If
					Else
						'If inverted=1 or HorizontalInversion = 1 then
						'paso de inverted
						If HorizontalInversion = 1 then
							dValue=DoubleField(length - dValor1,5)
						Else				
							dValue=DoubleField(dValor1,5)
						End If
						
					End If
					If dValorY > 0  Then
						'dValueY="-" & DoubleField(dvalorY,4)
						'modificamos el 15-12-05
						dValueY= DoubleField(dvalorY,5)
					Else
						dValueY=""
					End if
					
					'If m=n-1 then
					'modifica javi para controlar solo las operaciones validas (primitivas)
					If m=intContUltOpVal then
						'si es la Ãºltima operaciÃ³n
						'strTool=strName & dValue & DoubleField(dValorZ,5)
						strTool=strName & dValue & DoubleField(dValorZ,5) & dValueY
						Tools=Tools & strTool & vbCrLf
					Else 
						'sino es la Ãºltima operaciÃ³n
						'strTool=strName & dValue & DoubleField(dValorZ,5)  & "C" 
						strTool=strName & dValue & DoubleField(dValorZ,5)  &  dValueY & "C" 						
						Tools=Tools & strTool & vbCrLf
					End If 
				End If 
			Next
			
		End Function
    		
		Public Function Operations(OperationsList)
			dim strName
			dim objOp
			dim n,j,i
			n=OperationsList.length
			i=0
			
			'PARA CONTAR LASOPERACIONES QUE REALMENTE SON PRIMITIVAS >>> LAS QUE SE VAN A ESCRIBIR EN EL CENTRO
			For j=0 to n-1
				set objOp=OperationsList.Item(j)
				strName= objOp.getAttribute("nameInMachine")
				if strName = "" then
					strName=objOp.getAttribute("name")
				end if
				'If Left(strName,1)="W" Then
				If Left(strName,1)="W"  and Len(strName)=8 Then
					i=i+1
				End If
			Next 
			
			If i=0 Then    				
				Operations=""
			Else
				Operations="C"
			End If
		End Function 
		
		'AÃ±adimos el 15-12-2005
		Function GetReferencesinGuion(Referencee)
			GetReferencesinGuion=Left(Referencee,2) & Mid(Referencee,4,5)
		End Function
		
		'AÃ±adida el 9-4-2006 para obtener la referencia a partir de la referencia final.
		Function GetReferenceBasesinGuion(finalReferencee)
			GetReferenceBasesinGuion=Mid(finalReferencee,4,2) & Mid(finalReferencee,7,5)
		End Function
		
		Function GetSturtzCode(finalReference)
				'GetSturtzCode=left(finalReference,7) + "000" + Right(finalReference,3)
				 GetSturtzCode=finalReference
		End Function
   		   		
   		Function SetLocaleFirst()
		  Dim original
		  original = SetLocale("en-gb")
		  SetLocaleFirst = ""
		End Function
	
		Function SetLocaleEnd()
			Dim original
			original = SetLocale(currentLocale)
			SetLocaleEnd = ""
		End Function  		

    		
		]]></msxsl:script>
    <xsl:param name="set" select="1" />
    <xsl:param name="machine" select="1" />
    <xsl:template match="/">
        <xsl:apply-templates
            select="ProductionLot/ProductionSet[@ProductionSetNumber=$set]/Machine[@machineId=$machine]" />
    </xsl:template>
    <xsl:template match="Machine">
        <xsl:value-of select="script:SetLocaleFirst()" />
        <!-- cabecera de fichero -->
        <xsl:text>THOR020</xsl:text>
        <xsl:text>&#xA;</xsl:text>
        <xsl:for-each select="Rod">
            <xsl:if
                test="($machine!=17 and not(@reference='GAR8832')) or ($machine=17 and @reference='MARCOREF' or @reference='HOJAREF' or @reference='HOJAREFBALC')">
                <xsl:value-of select="script:ResetPN()" />
                <xsl:for-each select="Piece">
                    <xsl:sort select="@absoluteNumber" order="ascending" />
                    <xsl:if test="count(Operations/Operation[@nameInMachine])>0">
                        <xsl:variable name="numTr" select="format-number(script:IncKTN(),'0000')" />
                        <xsl:variable name="PN1" select="script:IncPN()" />
                        <xsl:variable name="CajeadosPuerta"
                            select="count(Operations/Operation[@nameInMachine = 724 or @nameInMachine = 725 or @nameInMachine = 736 or @nameInMachine = 737 or @nameInMachine = 748 or @nameInMachine = 749])>0" />
                        <xsl:variable name="Bombillo"
                            select="count(Operations/Operation[@nameInMachine = 780 or @nameInMachine = 782 or @nameInMachine = 784 or @nameInMachine = 786 or @nameInMachine = 787 or @nameInMachine = 788])>0" />
                        <xsl:variable name="Maneta"
                            select="count(Operations/Operation[@nameInMachine = 500 or @nameInMachine = 700])>0" />
                        <xsl:variable name="TaladroGSNRE"
                            select="count(Operations/Operation[@nameInMachine = 406])>0" />
                        <xsl:variable name="DosPasadas"
                            select="count(Operations/Operation[@nameInMachine = 320 or @nameInMachine = 322 or @nameInMachine = 450 or @nameInMachine = 452])>0" />
                        <xsl:text>KTN</xsl:text>
                        <xsl:value-of select="format-number(@absoluteNumber,'0000')" />
                        <xsl:text>C</xsl:text>
                        <xsl:choose>
                            <xsl:when test="@container>0">
                                <xsl:value-of select="format-number(@container,'00')" />
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:text>00</xsl:text>
                            </xsl:otherwise>
                        </xsl:choose>
                        <xsl:text>F</xsl:text>
                        <xsl:choose>
                            <xsl:when test="@slot>0">
                                <xsl:value-of select="format-number(@slot,'000')" />
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:text>000</xsl:text>
                            </xsl:otherwise>
                        </xsl:choose>
                        <xsl:text>001</xsl:text>
                        <xsl:value-of
                            select="concat('K',format-number(ancestor::ProductionLot/@ProductionLot,'000000'),format-number($set,'00'))" />
                        <xsl:value-of
                            select="concat('P',format-number(@pieceNumber,'00'),format-number(parent::Rod/@number,'000'))" />
                        <!-- Positionsname xxxyyyzzz = x(absolutepiecenumber) y(0) z(0) -->
                        <xsl:value-of select="concat('E','00')" />
                        <xsl:text>T</xsl:text>
                        <xsl:choose>
                            <xsl:when test="parent::Rod/@reference='GUIAPVC30X60'">
                                <xsl:value-of select="script:Derecha('30X60',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GUIAPVC30X60+0'">
                                <xsl:value-of select="script:Derecha('30X60',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GUIAPVC30X60+PROL'">
                                <xsl:value-of select="script:Derecha('30X601P',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GUIAPVC30X60+2PROL'">
                                <xsl:value-of select="script:Derecha('30X602P',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='H-20+0'">
                                <xsl:value-of select="script:Derecha('H-20',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='H-20+PROL'">
                                <xsl:value-of select="script:Derecha('H-201P',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='H-20+2PROL'">
                                <xsl:value-of select="script:Derecha('H-202P',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='G30X60+PROL_VIUDA'">
                                <xsl:value-of select="script:Derecha('30X601P',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='G30X60+2PROL_VIUDA'">
                                <xsl:value-of select="script:Derecha('30X602P',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GUIA30X60VIUDA+0'">
                                <xsl:value-of select="script:Derecha('GA30X60',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GUIA30X60LA_VIUDA'">
                                <xsl:value-of select="script:Derecha('GA30X60',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GUIAPVC78'">
                                <xsl:value-of select="script:Derecha('78',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GUIAPVC78+PROL'">
                                <xsl:value-of select="script:Derecha('78',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GAR8115FI'">
                                <xsl:value-of select="script:Derecha('GAR8115',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GAR8115BP'">
                                <xsl:value-of select="script:Derecha('GAR8115',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GAR8101F2C'">
                                <xsl:value-of select="script:Derecha('GAR8101F',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GAR8107F2C'">
                                <xsl:value-of select="script:Derecha('GAR8107F',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GAR8108F2C'">
                                <xsl:value-of select="script:Derecha('GAR8108F',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GAR8110F2C'">
                                <xsl:value-of select="script:Derecha('GAR8110F',8,string(s))" />
                            </xsl:when>
                            <xsl:when test="parent::Rod/@reference='GAR8201F2C'">
                                <xsl:value-of select="script:Derecha('GAR8201F',8,string(s))" />
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:value-of
                                    select="script:Derecha(script:GetSturtzCode(string(parent::Rod/@reference)),8,string(s))" />
                            </xsl:otherwise>
                        </xsl:choose>

                        <xsl:text>V</xsl:text>
                        <xsl:value-of
                            select="script:Derecha(string(parent::Rod/@color),20,string(s))" />
                        <xsl:text>I</xsl:text>
                        <xsl:value-of
                            select="script:Izquierda(string(Steels/Steel/@reference),8,string(s))" />
                        <xsl:text>M</xsl:text>
                        <xsl:choose>
                            <xsl:when test="Steels/Steel/@length*10>0">
                                <xsl:value-of
                                    select="script:Izquierda(string(Steels/Steel/@length*10),5,'z')" />
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:text>00000</xsl:text>
                            </xsl:otherwise>
                        </xsl:choose>
                        <xsl:text>D00</xsl:text>
                        <xsl:text>U</xsl:text>
                        <xsl:value-of
                            select="script:GetSituation(string(@angle),string(parent::Rod/@rol))" />
                        <xsl:text>A00</xsl:text>
                        <xsl:text>L</xsl:text>
                        <xsl:value-of select="script:Izquierda(string(@length*10),5,'z')" />
                        <xsl:text>G</xsl:text>
                        <xsl:choose>
                            <xsl:when test="parent::Rod/@rol='SASH'">
                                <xsl:choose>
                                    <xsl:when test="number(@angleA)=90 and number(@angleB)=45">
                                        <xsl:value-of
                                            select="script:GetAngles(string(@angleA),string(@angleB+90),string(parent::Rod/@inverted))" />
                                    </xsl:when>
                                    <xsl:when test="number(@angleA)=45 and number(@angleB)=90">
                                        <xsl:value-of
                                            select="script:GetAngles(string(@angleA+90),string(@angleB),string(parent::Rod/@inverted))" />
                                    </xsl:when>
                                    <xsl:when test="number(@angleA)=90 and number(@angleB)=90">
                                        <xsl:value-of
                                            select="script:GetAngles(string(@angleA),string(@angleB),string(parent::Rod/@inverted))" />
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:value-of
                                            select="script:GetAngles(string(@angleA+90),string(@angleB+90),string(parent::Rod/@inverted))" />
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:value-of
                                    select="script:GetAngles(string(@angleB),string(@angleA),string(parent::Rod/@inverted))" />
                            </xsl:otherwise>
                        </xsl:choose>
                        <xsl:text>J</xsl:text>
                        <xsl:value-of select="script:GetAngles(string(@angleA),string(@angleB),'0')" />
                        <xsl:text>B</xsl:text>
                        <xsl:value-of
                            select="script:Derecha(translate(string(parent::Rod/@Description), 'ñçáéíóúàèìòùªºÑÇÁÉÍÓÚÀÈÌÒÙ´,', 'ncaeiouaeiouaoNCAEIOUAEIOU*;'),20,string(s))" />
                        <xsl:text>H</xsl:text>
                        <xsl:choose>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),7,'z')='GAR8822'">
                                <xsl:text>Deceuninck          </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),7,'z')='GAR8832'">
                                <xsl:text>Deceuninck          </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),3,'z')='GAR'">
                                <xsl:text>Wingo               </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),3,'z')='DEC'">
                                <xsl:text>Deceuninck          </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),17,'z')='GUIA30X60LA_VIUDA'">
                                <xsl:text>LaViuda             </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),16,'z')='GUIA30X60VIUDA+0'">
                                <xsl:text>LaViuda             </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),12,'z')='GUIAPVC30X60'">
                                <xsl:text>GimenezGanga        </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),12,'z')='GPVC30X60+0P'">
                                <xsl:text>GimenezGanga        </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),12,'z')='GPVC30X60+1P'">
                                <xsl:text>GimenezGanga        </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),12,'z')='GPVC30X60+2P'">
                                <xsl:text>GimenezGanga        </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),14,'z')='GUIAPVC30X60+0'">
                                <xsl:text>GimenezGanga        </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),6,'z')='G30X60'">
                                <xsl:text>GimenezGanga        </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),9,'z')='GUIAPVC78'">
                                <xsl:text>Mocaplas            </xsl:text>
                            </xsl:when>
                            <xsl:when
                                test="script:Izquierda(string(parent::Rod/@finalReference),9,'z')='GUIAPVC78+PROL'">
                                <xsl:text>Mocaplas            </xsl:text>
                            </xsl:when>
                            <xsl:when test="@system='RE 70/ALU/C30'">
                                <xsl:text>REFINE              </xsl:text>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:text>Regicarp            </xsl:text>
                            </xsl:otherwise>
                        </xsl:choose>
                        <xsl:text>O</xsl:text>
                        <xsl:variable name="prefCimBC"
                            select="concat(format-number(ancestor::ProductionLot/@ProductionLot,'0000'),format-number($set,'00'),format-number($machine,'00'),format-number(@absoluteNumber,'0000'))" />
                        <xsl:value-of select="script:Izquierda(string($prefCimBC),30,string(s))" />
                        <xsl:text>SN+000N+000RN+000N+000</xsl:text>
                        <xsl:text>&#xA;</xsl:text>

                        <xsl:variable name="InvertirSturtz">
                            <xsl:choose>
                                <xsl:when test="parent::Rod/@reference='DEC3465'">
                                    <xsl:value-of select="0" />
                                </xsl:when>
                                <!--<xsl:when
                            test="parent::Rod/@reference='GAR1202'">	
									<xsl:value-of select="1"/>							
								</xsl:when>-->
                                <xsl:when test="parent::Rod/@reference='GAR7114'">
                                    <xsl:value-of select="1" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR7180'">
                                    <xsl:value-of select="1" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='DEC3483'">
                                    <xsl:value-of select="1" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='HOJAREF'">
                                    <xsl:value-of select="0" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR8822'">
                                    <xsl:value-of select="0" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR5102'">
                                    <xsl:value-of select="0" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@rol='SASH'">
                                    <xsl:value-of select="1" />
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of select="0" />
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:variable>
                        <xsl:if test="$DosPasadas">
                            <xsl:text>ZP02 ATENCION 1a PASADA SIN REFUERZO</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                            <xsl:text>ZP03 EL TRAMO NO DEBE LLEVAR REFUERZO</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                        </xsl:if>
                        <xsl:if test="$CajeadosPuerta">
                            <xsl:text>ZP02 *** CAJEADOS DE PUERTA ***</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                            <xsl:text>ZP03 *** QUITAR HIERRO ***</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                        </xsl:if>
                        <xsl:if test="$Bombillo">
                            <xsl:text>ZP02 *** BOMBILLO ***</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                            <xsl:text>ZP03 *** MECANIZAR EN LA P104 ***</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                        </xsl:if>
                        <xsl:if test="$Maneta and parent::Rod/@reference='GAR8115'">
                            <xsl:text>ZP02 *** MANETA EN HOJA OCULTA ***</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                            <xsl:text>ZP03 *** MECANIZAR LA MANETA EN LA P104 ***</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                        </xsl:if>
                        <xsl:if
                            test="$TaladroGSNRE and (parent::Rod/@reference='GAR8822' or parent::Rod/@reference='GAR8822N')">
                            <xsl:text>ZP02 *** TALADRO GSNRE ***</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                            <xsl:text>ZP03 *** MECANIZAR EN LA P104 LA OPERACION 406: TALADRO GSNRE ***</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                        </xsl:if>
                        <xsl:choose>
                            <xsl:when test="$InvertirSturtz=1">
                                <!-- INVERTIRDOS _________________________TODOS LOS MECANIZADOS
                                MENOS
                        ATORNILLADOS____________________________________ -->
                                <xsl:for-each select="Operations/Operation">
                                    <xsl:sort select="MachineSystem/@X" order="descending"
                                        data-type="number"></xsl:sort>
                                    <xsl:variable name="Nombre">
                                        <xsl:choose>
                                            <xsl:when test="@nameInMachine">
                                                <xsl:value-of select="@nameInMachine"></xsl:value-of>
                                            </xsl:when>
                                            <xsl:otherwise>
                                                <xsl:value-of select="@name"></xsl:value-of>
                                            </xsl:otherwise>
                                        </xsl:choose>
                                    </xsl:variable>
                                    <xsl:if test="$Nombre=320 and not($DosPasadas)">
                                        <xsl:text>ZP02 *** Cajeado de recojedor ***</xsl:text>
                                        <xsl:text>&#xA;</xsl:text>
                                        <xsl:text>ZP03 Refuerzos de aprox.: </xsl:text>
                                        <xsl:value-of select="MachineSystem/@X - 71 - 41 - 10 - 6" />
                                        <xsl:text> y </xsl:text>
                                        <xsl:value-of
                                            select="ancestor::Piece/@length - MachineSystem/@X - 71 - 41 - 10 - 6" />
                                        <xsl:text>&#xA;</xsl:text>
                                    </xsl:if>
                                    <xsl:if
                                        test="string-length($Nombre)=3 and (number($Nombre)>100 or number($Nombre)=100) and (not($DosPasadas) or $Nombre=320 or $Nombre=322 or $Nombre=450 or $Nombre=452)">
                                        <xsl:variable name="name" select="substring($Nombre,1,1)" />
                                        <xsl:if
                                            test="($name='0' or $name='1' or $name='2'  or $name='3'  or $name='4'  or $name='5'  or $name='6'  or $name='7'  or $name='8' or $name='9') and (MachineSystem/@X*10>=0) and (ancestor::Piece/@length*10>=MachineSystem/@X*10)">
                                            <xsl:text>W</xsl:text>
                                            <xsl:value-of select="$Nombre" />
                                            <xsl:text>/</xsl:text>
                                            <xsl:text>00</xsl:text>
                                            <xsl:text>/</xsl:text>
                                            <xsl:variable name="IX"
                                                select="ancestor::Piece/@length+(-1)*MachineSystem/@X" />
                                            <xsl:value-of select="format-number($IX*10,'00000')" />
                                            <xsl:text>/</xsl:text>
                                            <xsl:choose>
                                                <xsl:when
                                                    test="Variable/@name='Milling' or Variable/@name='Aguja'">
                                                    <xsl:value-of
                                                        select="format-number(Variable/@value*10,'00000')" />
                                                </xsl:when>
                                                <xsl:otherwise>
                                                    <xsl:text>00000</xsl:text>
                                                </xsl:otherwise>
                                            </xsl:choose>
                                            <xsl:text>&#xA;</xsl:text>
                                        </xsl:if>
                                    </xsl:if>
                                </xsl:for-each>
                                <!--
                        _________________________ATORNILLADOS____________________________________ -->
                                <xsl:if test="not($DosPasadas)">
                                    <xsl:for-each select="Operations/Operation">
                                        <xsl:sort select="Operations/Operation/MachineSystem/@X"
                                            order="ascending" />
                                        <xsl:sort select="MachineSystem/@X" order="descending"
                                            data-type="number"></xsl:sort>
                                        <xsl:variable name="Nombre">
                                            <xsl:choose>
                                                <xsl:when test="@nameInMachine">
                                                    <xsl:value-of select="@nameInMachine"></xsl:value-of>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                    <xsl:value-of select="@name"></xsl:value-of>
                                                </xsl:otherwise>
                                            </xsl:choose>
                                        </xsl:variable>
                                        <xsl:if
                                            test="string-length($Nombre)=3 and 100>number($Nombre) and ancestor::Piece/@length+(-1)*MachineSystem/@X > 70">
                                            <xsl:variable name="name"
                                                select="substring($Nombre,1,1)" />
                                            <xsl:if
                                                test="($name='0' or $name='1' or $name='2'  or $name='3'  or $name='4'  or $name='5'  or $name='6'  or $name='7'  or $name='8' or $name='9') and (MachineSystem/@X*10>=0) and (ancestor::Piece/@length*10>=MachineSystem/@X*10)">
                                                <xsl:text>W</xsl:text>
                                                <xsl:value-of select="$Nombre" />
                                                <xsl:text>/</xsl:text>
                                                <xsl:text>00</xsl:text>
                                                <xsl:text>/</xsl:text>
                                                <xsl:variable name="IX"
                                                    select="ancestor::Piece/@length+(-1)*MachineSystem/@X" />
                                                <xsl:value-of select="format-number($IX*10,'00000')" />
                                                <xsl:text>/</xsl:text>
                                                <xsl:choose>
                                                    <xsl:when
                                                        test="Variable/@name='Milling' or Variable/@name='Aguja'">
                                                        <xsl:value-of
                                                            select="format-number(Variable/@value*10,'00000')" />
                                                    </xsl:when>
                                                    <xsl:otherwise>
                                                        <xsl:text>00000</xsl:text>
                                                    </xsl:otherwise>
                                                </xsl:choose>
                                                <xsl:text>&#xA;</xsl:text>
                                            </xsl:if>
                                        </xsl:if>
                                    </xsl:for-each>
                                </xsl:if>
                            </xsl:when>
                            <xsl:otherwise>
                                <!-- NO INVERTIDOS _________________________TODOS LOS MECANIZADOS
                                MENOS
                        ATORNILLADOS____________________________________ -->
                                <xsl:for-each select="Operations/Operation">
                                    <xsl:sort select="MachineSystem/@X" order="ascending"
                                        data-type="number"></xsl:sort>
                                    <xsl:variable name="Nombre">
                                        <xsl:choose>
                                            <xsl:when test="@nameInMachine">
                                                <xsl:value-of select="@nameInMachine"></xsl:value-of>
                                            </xsl:when>
                                            <xsl:otherwise>
                                                <xsl:value-of select="@name"></xsl:value-of>
                                            </xsl:otherwise>
                                        </xsl:choose>
                                    </xsl:variable>
                                    <xsl:if test="$Nombre=320 and not($DosPasadas)">
                                        <xsl:text>ZP02 *** Cajeado de recojedor ***</xsl:text>
                                        <xsl:text>&#xA;</xsl:text>
                                        <xsl:text>ZP03 Refuerzos de aprox.: </xsl:text>
                                        <xsl:value-of select="MachineSystem/@X - 71 - 41 - 10 - 6" />
                                        <xsl:text> y </xsl:text>
                                        <xsl:value-of
                                            select="ancestor::Piece/@length - MachineSystem/@X - 71 - 41 - 10 - 6" />
                                        <xsl:text>&#xA;</xsl:text>
                                    </xsl:if>
                                    <xsl:if
                                        test="string-length($Nombre)=3 and (number($Nombre)>100 or number($Nombre)=100) and (not($DosPasadas) or $Nombre=320 or $Nombre=322 or $Nombre=450 or $Nombre=452)">
                                        <xsl:variable name="name" select="substring($Nombre,1,1)" />
                                        <xsl:if
                                            test="($name='0' or $name='1' or $name='2'  or $name='3'  or $name='4'  or $name='5'  or $name='6'  or $name='7'  or $name='8' or $name='9') and (MachineSystem/@X*10>=0) and (ancestor::Piece/@length*10>=MachineSystem/@X*10)">
                                            <xsl:text>W</xsl:text>
                                            <xsl:value-of select="$Nombre" />
                                            <xsl:text>/</xsl:text>
                                            <xsl:text>00</xsl:text>
                                            <xsl:text>/</xsl:text>
                                            <xsl:value-of
                                                select="format-number(MachineSystem/@X*10,'00000')" />
                                            <xsl:text>/</xsl:text>
                                            <xsl:choose>
                                                <xsl:when
                                                    test="Variable/@name='Milling' or Variable/@name='Aguja'">
                                                    <xsl:value-of
                                                        select="format-number(Variable/@value*10,'00000')" />
                                                </xsl:when>
                                                <xsl:otherwise>
                                                    <xsl:text>00000</xsl:text>
                                                </xsl:otherwise>
                                            </xsl:choose>
                                            <xsl:text>&#xA;</xsl:text>
                                        </xsl:if>
                                    </xsl:if>
                                </xsl:for-each>
                                <!--
                        _________________________ATORNILLADOS____________________________________ -->
                                <xsl:if test="not($DosPasadas)">
                                    <xsl:for-each select="Operations/Operation">
                                        <xsl:sort select="MachineSystem/@X" order="ascending"
                                            data-type="number"></xsl:sort>
                                        <xsl:variable name="Nombre">
                                            <xsl:choose>
                                                <xsl:when test="@nameInMachine">
                                                    <xsl:value-of select="@nameInMachine"></xsl:value-of>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                    <xsl:value-of select="@name"></xsl:value-of>
                                                </xsl:otherwise>
                                            </xsl:choose>
                                        </xsl:variable>
                                        <xsl:if
                                            test="string-length($Nombre)=3 and 100>number($Nombre) and MachineSystem/@X>70">
                                            <xsl:variable name="name"
                                                select="substring($Nombre,1,1)" />
                                            <xsl:if
                                                test="($name='0' or $name='1' or $name='2'  or $name='3'  or $name='4'  or $name='5'  or $name='6'  or $name='7'  or $name='8' or $name='9') and (MachineSystem/@X*10>=0) and (ancestor::Piece/@length*10>=MachineSystem/@X*10)">
                                                <xsl:text>W</xsl:text>
                                                <xsl:value-of select="$Nombre" />
                                                <xsl:text>/</xsl:text>
                                                <xsl:text>00</xsl:text>
                                                <xsl:text>/</xsl:text>
                                                <xsl:value-of
                                                    select="format-number(MachineSystem/@X*10,'00000')" />
                                                <xsl:text>/</xsl:text>
                                                <xsl:choose>
                                                    <xsl:when
                                                        test="Variable/@name='Milling' or Variable/@name='Aguja'">
                                                        <xsl:value-of
                                                            select="format-number(Variable/@value*10,'00000')" />
                                                    </xsl:when>
                                                    <xsl:otherwise>
                                                        <xsl:text>00000</xsl:text>
                                                    </xsl:otherwise>
                                                </xsl:choose>
                                                <xsl:text>&#xA;</xsl:text>
                                            </xsl:if>
                                        </xsl:if>
                                    </xsl:for-each>
                                </xsl:if>
                            </xsl:otherwise>
                        </xsl:choose>
                        <!-- ========================= 2a PASADA (solo atornillados)
                ========================= -->
                        <xsl:if test="$DosPasadas">
                            <xsl:text>KTN</xsl:text>
                            <xsl:value-of select="format-number(@absoluteNumber,'0000')" />
                            <xsl:text>C</xsl:text>
                            <xsl:choose>
                                <xsl:when test="@container>0">
                                    <xsl:value-of select="format-number(@container,'00')" />
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:text>00</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                            <xsl:text>F</xsl:text>
                            <xsl:choose>
                                <xsl:when test="@slot>0">
                                    <xsl:value-of select="format-number(@slot,'000')" />
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:text>000</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                            <xsl:text>001</xsl:text>
                            <xsl:value-of
                                select="concat('K',format-number(ancestor::ProductionLot/@ProductionLot,'000000'),format-number($set,'00'))" />
                            <xsl:value-of
                                select="concat('P',format-number(@pieceNumber,'00'),format-number(parent::Rod/@number,'000'))" />
                            <xsl:value-of select="concat('E','00')" />
                            <xsl:text>T</xsl:text>
                            <xsl:choose>
                                <xsl:when test="parent::Rod/@reference='GUIAPVC30X60'">
                                    <xsl:value-of select="script:Derecha('30X60',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GUIAPVC30X60+0'">
                                    <xsl:value-of select="script:Derecha('30X60',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GUIAPVC30X60+PROL'">
                                    <xsl:value-of select="script:Derecha('30X601P',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GUIAPVC30X60+2PROL'">
                                    <xsl:value-of select="script:Derecha('30X602P',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='H-20+0'">
                                    <xsl:value-of select="script:Derecha('H-20',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='H-20+PROL'">
                                    <xsl:value-of select="script:Derecha('H-201P',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='H-20+2PROL'">
                                    <xsl:value-of select="script:Derecha('H-202P',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='G30X60+PROL_VIUDA'">
                                    <xsl:value-of select="script:Derecha('30X601P',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='G30X60+2PROL_VIUDA'">
                                    <xsl:value-of select="script:Derecha('30X602P',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GUIA30X60VIUDA+0'">
                                    <xsl:value-of select="script:Derecha('GA30X60',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GUIA30X60LA_VIUDA'">
                                    <xsl:value-of select="script:Derecha('GA30X60',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GUIAPVC78'">
                                    <xsl:value-of select="script:Derecha('78',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GUIAPVC78+PROL'">
                                    <xsl:value-of select="script:Derecha('78',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR8115FI'">
                                    <xsl:value-of select="script:Derecha('GAR8115',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR8115BP'">
                                    <xsl:value-of select="script:Derecha('GAR8115',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR8101F2C'">
                                    <xsl:value-of select="script:Derecha('GAR8101F',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR8107F2C'">
                                    <xsl:value-of select="script:Derecha('GAR8107F',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR8108F2C'">
                                    <xsl:value-of select="script:Derecha('GAR8108F',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR8110F2C'">
                                    <xsl:value-of select="script:Derecha('GAR8110F',8,string(s))" />
                                </xsl:when>
                                <xsl:when test="parent::Rod/@reference='GAR8201F2C'">
                                    <xsl:value-of select="script:Derecha('GAR8201F',8,string(s))" />
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of
                                        select="script:Derecha(script:GetSturtzCode(string(parent::Rod/@reference)),8,string(s))" />
                                </xsl:otherwise>
                            </xsl:choose>
                            <xsl:text>V</xsl:text>
                            <xsl:value-of
                                select="script:Derecha(string(parent::Rod/@color),20,string(s))" />
                            <xsl:text>I</xsl:text>
                            <xsl:value-of
                                select="script:Izquierda(string(Steels/Steel/@reference),8,string(s))" />
                            <xsl:text>M</xsl:text>
                            <xsl:choose>
                                <xsl:when test="Steels/Steel/@length*10>0">
                                    <xsl:value-of
                                        select="script:Izquierda(string(Steels/Steel/@length*10),5,'z')" />
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:text>00000</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                            <xsl:text>D00</xsl:text>
                            <xsl:text>U</xsl:text>
                            <xsl:value-of
                                select="script:GetSituation(string(@angle),string(parent::Rod/@rol))" />
                            <xsl:text>A00</xsl:text>
                            <xsl:text>L</xsl:text>
                            <xsl:value-of select="script:Izquierda(string(@length*10),5,'z')" />
                            <xsl:text>G</xsl:text>
                            <xsl:choose>
                                <xsl:when test="parent::Rod/@rol='SASH'">
                                    <xsl:choose>
                                        <xsl:when test="number(@angleA)=90 and number(@angleB)=45">
                                            <xsl:value-of
                                                select="script:GetAngles(string(@angleA),string(@angleB+90),string(parent::Rod/@inverted))" />
                                        </xsl:when>
                                        <xsl:when test="number(@angleA)=45 and number(@angleB)=90">
                                            <xsl:value-of
                                                select="script:GetAngles(string(@angleA+90),string(@angleB),string(parent::Rod/@inverted))" />
                                        </xsl:when>
                                        <xsl:when test="number(@angleA)=90 and number(@angleB)=90">
                                            <xsl:value-of
                                                select="script:GetAngles(string(@angleA),string(@angleB),string(parent::Rod/@inverted))" />
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <xsl:value-of
                                                select="script:GetAngles(string(@angleA+90),string(@angleB+90),string(parent::Rod/@inverted))" />
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of
                                        select="script:GetAngles(string(@angleB),string(@angleA),string(parent::Rod/@inverted))" />
                                </xsl:otherwise>
                            </xsl:choose>
                            <xsl:text>J</xsl:text>
                            <xsl:value-of
                                select="script:GetAngles(string(@angleA),string(@angleB),'0')" />
                            <xsl:text>B</xsl:text>
                            <xsl:value-of
                                select="script:Derecha(translate(string(parent::Rod/@Description), 'ñçáéíóúàèìòùªºÑÇÁÉÍÓÚÀÈÌÒÙ´,', 'ncaeiouaeiouaoNCAEIOUAEIOU*;'),20,string(s))" />
                            <xsl:text>H</xsl:text>
                            <xsl:choose>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),7,'z')='GAR8822'">
                                    <xsl:text>Deceuninck          </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),7,'z')='GAR8832'">
                                    <xsl:text>Deceuninck          </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),3,'z')='GAR'">
                                    <xsl:text>Wingo               </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),3,'z')='DEC'">
                                    <xsl:text>Deceuninck          </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),17,'z')='GUIA30X60LA_VIUDA'">
                                    <xsl:text>LaViuda             </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),16,'z')='GUIA30X60VIUDA+0'">
                                    <xsl:text>LaViuda             </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),12,'z')='GUIAPVC30X60'">
                                    <xsl:text>GimenezGanga        </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),12,'z')='GPVC30X60+0P'">
                                    <xsl:text>GimenezGanga        </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),12,'z')='GPVC30X60+1P'">
                                    <xsl:text>GimenezGanga        </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),12,'z')='GPVC30X60+2P'">
                                    <xsl:text>GimenezGanga        </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),14,'z')='GUIAPVC30X60+0'">
                                    <xsl:text>GimenezGanga        </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),6,'z')='G30X60'">
                                    <xsl:text>GimenezGanga        </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),9,'z')='GUIAPVC78'">
                                    <xsl:text>Mocaplas            </xsl:text>
                                </xsl:when>
                                <xsl:when
                                    test="script:Izquierda(string(parent::Rod/@finalReference),9,'z')='GUIAPVC78+PROL'">
                                    <xsl:text>Mocaplas            </xsl:text>
                                </xsl:when>
                                <xsl:when test="@system='RE 70/ALU/C30'">
                                    <xsl:text>REFINE              </xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:text>Regicarp            </xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                            <xsl:text>O</xsl:text>
                            <xsl:variable name="prefCimBC2"
                                select="concat(format-number(ancestor::ProductionLot/@ProductionLot,'0000'),format-number($set,'00'),format-number($machine,'00'),format-number(@absoluteNumber,'0000'))" />
                            <xsl:value-of
                                select="script:Izquierda(string($prefCimBC2),30,string(s))" />
                            <xsl:text>SN+000N+000RN+000N+000</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                            <xsl:text>ZP02 ATENCION 2a PASADA</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                            <xsl:text>ZP03 MECANIZAR CON REFUERZO</xsl:text>
                            <xsl:text>&#xA;</xsl:text>
                            <!-- Mecanizados de la 2a pasada (todo excepto 320 y 450) -->
                            <xsl:choose>
                                <xsl:when test="$InvertirSturtz=1">
                                    <!-- INVERTIDOS 2a pasada -->
                                    <xsl:for-each select="Operations/Operation">
                                        <xsl:sort select="MachineSystem/@X" order="descending"
                                            data-type="number"></xsl:sort>
                                        <xsl:variable name="Nombre2m">
                                            <xsl:choose>
                                                <xsl:when test="@nameInMachine">
                                                    <xsl:value-of select="@nameInMachine"></xsl:value-of>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                    <xsl:value-of select="@name"></xsl:value-of>
                                                </xsl:otherwise>
                                            </xsl:choose>
                                        </xsl:variable>
                                        <xsl:if
                                            test="string-length($Nombre2m)=3 and (number($Nombre2m)>100 or number($Nombre2m)=100) and $Nombre2m!=320 and $Nombre2m!=450 and $Nombre2m!=322 and $Nombre2m!=452">
                                            <xsl:variable name="name2m"
                                                select="substring($Nombre2m,1,1)" />
                                            <xsl:if
                                                test="($name2m='0' or $name2m='1' or $name2m='2' or $name2m='3' or $name2m='4' or $name2m='5' or $name2m='6' or $name2m='7' or $name2m='8' or $name2m='9') and (MachineSystem/@X*10>=0) and (ancestor::Piece/@length*10>=MachineSystem/@X*10)">
                                                <xsl:text>W</xsl:text>
                                                <xsl:value-of select="$Nombre2m" />
                                                <xsl:text>/</xsl:text>
                                                <xsl:text>00</xsl:text>
                                                <xsl:text>/</xsl:text>
                                                <xsl:variable name="IX2"
                                                    select="ancestor::Piece/@length + (-1)*MachineSystem/@X" />
                                                <xsl:value-of
                                                    select="format-number($IX2*10,'00000')" />
                                                <xsl:text>/</xsl:text>
                                                <xsl:choose>
                                                    <xsl:when
                                                        test="Variable/@name='Milling' or Variable/@name='Aguja'">
                                                        <xsl:value-of
                                                            select="format-number(Variable/@value*10,'00000')" />
                                                    </xsl:when>
                                                    <xsl:otherwise>
                                                        <xsl:text>00000</xsl:text>
                                                    </xsl:otherwise>
                                                </xsl:choose>
                                                <xsl:text>&#xA;</xsl:text>
                                            </xsl:if>
                                        </xsl:if>
                                    </xsl:for-each>
                                </xsl:when>
                                <xsl:otherwise>
                                    <!-- NO INVERTIDOS 2a pasada -->
                                    <xsl:for-each select="Operations/Operation">
                                        <xsl:sort select="MachineSystem/@X" order="ascending"
                                            data-type="number"></xsl:sort>
                                        <xsl:variable name="Nombre2m">
                                            <xsl:choose>
                                                <xsl:when test="@nameInMachine">
                                                    <xsl:value-of select="@nameInMachine"></xsl:value-of>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                    <xsl:value-of select="@name"></xsl:value-of>
                                                </xsl:otherwise>
                                            </xsl:choose>
                                        </xsl:variable>
                                        <xsl:if
                                            test="string-length($Nombre2m)=3 and (number($Nombre2m)>100 or number($Nombre2m)=100) and $Nombre2m!=320 and $Nombre2m!=450 and $Nombre2m!=322 and $Nombre2m!=452">
                                            <xsl:variable name="name2m"
                                                select="substring($Nombre2m,1,1)" />
                                            <xsl:if
                                                test="($name2m='0' or $name2m='1' or $name2m='2' or $name2m='3' or $name2m='4' or $name2m='5' or $name2m='6' or $name2m='7' or $name2m='8' or $name2m='9') and (MachineSystem/@X*10>=0) and (ancestor::Piece/@length*10>=MachineSystem/@X*10)">
                                                <xsl:text>W</xsl:text>
                                                <xsl:value-of select="$Nombre2m" />
                                                <xsl:text>/</xsl:text>
                                                <xsl:text>00</xsl:text>
                                                <xsl:text>/</xsl:text>
                                                <xsl:value-of
                                                    select="format-number(MachineSystem/@X*10,'00000')" />
                                                <xsl:text>/</xsl:text>
                                                <xsl:choose>
                                                    <xsl:when
                                                        test="Variable/@name='Milling' or Variable/@name='Aguja'">
                                                        <xsl:value-of
                                                            select="format-number(Variable/@value*10,'00000')" />
                                                    </xsl:when>
                                                    <xsl:otherwise>
                                                        <xsl:text>00000</xsl:text>
                                                    </xsl:otherwise>
                                                </xsl:choose>
                                                <xsl:text>&#xA;</xsl:text>
                                            </xsl:if>
                                        </xsl:if>
                                    </xsl:for-each>
                                </xsl:otherwise>
                            </xsl:choose>
                            <!-- Atornillados de la 2a pasada -->
                            <xsl:for-each select="Operations/Operation">
                                <xsl:sort select="MachineSystem/@X" order="ascending"
                                    data-type="number"></xsl:sort>
                                <xsl:variable name="Nombre2">
                                    <xsl:choose>
                                        <xsl:when test="@nameInMachine">
                                            <xsl:value-of select="@nameInMachine"></xsl:value-of>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <xsl:value-of select="@name"></xsl:value-of>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </xsl:variable>
                                <xsl:if test="string-length($Nombre2)=3 and 100>number($Nombre2)">
                                    <xsl:variable name="name2" select="substring($Nombre2,1,1)" />
                                    <xsl:if
                                        test="($name2='0' or $name2='1' or $name2='2' or $name2='3' or $name2='4' or $name2='5' or $name2='6' or $name2='7' or $name2='8' or $name2='9') and (MachineSystem/@X*10>=0) and (ancestor::Piece/@length*10>=MachineSystem/@X*10)">
                                        <xsl:text>W</xsl:text>
                                        <xsl:value-of select="$Nombre2" />
                                        <xsl:text>/</xsl:text>
                                        <xsl:text>00</xsl:text>
                                        <xsl:text>/</xsl:text>
                                        <xsl:value-of
                                            select="format-number(MachineSystem/@X*10,'00000')" />
                                        <xsl:text>/</xsl:text>
                                        <xsl:choose>
                                            <xsl:when
                                                test="Variable/@name='Milling' or Variable/@name='Aguja'">
                                                <xsl:value-of
                                                    select="format-number(Variable/@value*10,'00000')" />
                                            </xsl:when>
                                            <xsl:otherwise>
                                                <xsl:text>00000</xsl:text>
                                            </xsl:otherwise>
                                        </xsl:choose>
                                        <xsl:text>&#xA;</xsl:text>
                                    </xsl:if>
                                </xsl:if>
                            </xsl:for-each>
                        </xsl:if>
                        <!-- ========================= FIN 2a PASADA ========================= -->
                    </xsl:if>
                </xsl:for-each>
            </xsl:if>
        </xsl:for-each>

        <xsl:value-of select="script:SetLocaleEnd()" />
    </xsl:template>
</xsl:stylesheet>