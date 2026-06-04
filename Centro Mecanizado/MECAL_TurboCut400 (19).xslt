<?xml version="1.0" encoding="utf-16"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
							  xmlns:math="urn:schemas-cagle-com:math"
							  xmlns:msxsl="urn:schemas-microsoft-com:xslt"
							  xmlns:vb_script="PB_VB"
							  xmlns:js_script="PB_JS">


	<xsl:output version="4.0" omit-xml-declaration="yes" indent="yes" method="text"/>
	<!-- _______________________________________________________ Root ____________________________________________________________ -->
	<!--Parameters-->
	<xsl:param name="set" select="1"/>
	<xsl:param name="machine" select="5"/>

	<!--Global variables-->
	<xsl:variable name="breakLine">
		<xsl:text>&#xA;</xsl:text>
	</xsl:variable>

	<xsl:template match="/">
		<xsl:apply-templates select="ProductionLot/ProductionSet[@ProductionSetNumber=$set]/Machine[@machineId=$machine]"/>
	</xsl:template>

	<xsl:template match="Machine">
		<xsl:text>id;ksn;blen;ktn;plen;l;r;pkodu;padi;genislik;yukseklik;araba;raf;konum;dskod;dslength;uretimno;teklifno;poz;bayi;musteri;renkK;renkA;pengenis;penyuksek;makro;barkod;resim</xsl:text>
		<xsl:value-of select="$breakLine" />
		<xsl:apply-templates select="descendant::Rod"/>
	</xsl:template>

	<xsl:template match="Rod">
		<xsl:apply-templates select="child::Piece[@length &gt; 370]">
			<xsl:with-param name="RodNumber" select="position()"></xsl:with-param>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="Piece">
	<xsl:param name="RodNumber"></xsl:param>
		<!--Piece Variables-->
		<xsl:variable name="barNumber" select="ancestor::Rod/@number"></xsl:variable>
		<xsl:variable name="barLength" select="format-number(ancestor::Rod/@length*10,'00000')"></xsl:variable>
		<xsl:variable name="barReference" select="ancestor::Rod/@reference"></xsl:variable>
		<xsl:variable name="squareId" select="@squareNumberInSet"></xsl:variable>
		<xsl:variable name="barCount">
			<xsl:number 
				level="any" 
				count="Piece[@length &gt; 370]" 
				from="Machine"/>
		</xsl:variable>
		
		<xsl:variable name="pieceCount">
			<xsl:number 
				level="single" 
				count="Piece[@length &gt; 370]" 
				from="Rod"/>
		</xsl:variable>
		
		<xsl:variable name="width">
			<xsl:choose>
				<xsl:when test="round(ancestor::ProductionSet/Machine[@machineId=$machine]/Rod[@rol!='MULLION' and @rol!='SASH STOP']/
				Piece[@squareNumberInSet=$squareId and (@angle=180 or @positionInSquare=2)]/@length - ancestor::ProductionSet/Machine[@machineId=$machine]/Rod[@rol!='MULLION' and @rol!='SASH STOP']/
				Piece[@squareNumberInSet=$squareId and (@angle=180 or @positionInSquare=2)]/@weldAddedB - ancestor::ProductionSet/Machine[@machineId=$machine]/Rod[@rol!='MULLION' and @rol!='SASH STOP']/
				Piece[@squareNumberInSet=$squareId and (@angle=180 or @positionInSquare=2)]/@weldAddedA) &gt; 0">
					<xsl:value-of select="round(ancestor::ProductionSet/Machine[@machineId=$machine]/Rod[@rol!='MULLION' and @rol!='SASH STOP']/
					Piece[@squareNumberInSet=$squareId and (@angle=180 or @positionInSquare=2)]/@length - ancestor::ProductionSet/Machine[@machineId=$machine]/Rod[@rol!='MULLION' and @rol!='SASH STOP']/
					Piece[@squareNumberInSet=$squareId and (@angle=180 or @positionInSquare=2)]/@weldAddedB - ancestor::ProductionSet/Machine[@machineId=$machine]/Rod[@rol!='MULLION' and @rol!='SASH STOP']/
					Piece[@squareNumberInSet=$squareId and (@angle=180 or @positionInSquare=2)]/@weldAddedA)"></xsl:value-of>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="0"></xsl:value-of>
				</xsl:otherwise>
			</xsl:choose>
			
		</xsl:variable>

		<xsl:variable name="height">
			<xsl:value-of select="round(ancestor::ProductionSet/Machine[@machineId=$machine]/Rod[@rol!='MULLION' and @rol!='SASH STOP']/
				Piece[@squareNumberInSet=$squareId and (@angle=270 or @positionInSquare=3)]/@length - ancestor::ProductionSet/Machine[@machineId=$machine]/Rod[@rol!='MULLION' and @rol!='SASH STOP']/
				Piece[@squareNumberInSet=$squareId and (@angle=270 or @positionInSquare=3)]/@weldAddedA - ancestor::ProductionSet/Machine[@machineId=$machine]/Rod[@rol!='MULLION' and @rol!='SASH STOP']/
				Piece[@squareNumberInSet=$squareId and (@angle=270 or @positionInSquare=3)]/@weldAddedB)"></xsl:value-of>
		</xsl:variable>

		<xsl:variable name="trolley">
			<xsl:choose>
				<xsl:when test="@container=-1">
					<xsl:text>0</xsl:text>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="@container"></xsl:value-of>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>

		<xsl:variable name="shelve">
			<xsl:choose>
				<xsl:when test="@slot=-1">
					<xsl:text>0</xsl:text>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="@slot" />
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>

		<xsl:variable name="position">
			<xsl:choose>
				<xsl:when test="@angle=360">
					<xsl:text>1</xsl:text>
				</xsl:when>
				<xsl:when test="@angle=90">
					<xsl:text>2</xsl:text>
				</xsl:when>
				<xsl:when test="@angle=180">
					<xsl:text>3</xsl:text>
				</xsl:when>
				<xsl:when test="@angle=270">
					<xsl:text>4</xsl:text>
				</xsl:when>
				<xsl:otherwise>
					<xsl:text>0</xsl:text>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>

		<xsl:variable name="barColorName">
			<xsl:call-template name="toUpper">
				<xsl:with-param name="text" select="ancestor::Rod/@color"/>
			</xsl:call-template>
		</xsl:variable>

		<xsl:variable name="colorCode">
			<xsl:choose>
				<xsl:when test="contains($barColorName,'WHITE') or contains($barColorName,'BLANCO') or contains($barColorName,'BRANCO')">
					<xsl:value-of select="11"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="14"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>

		<xsl:variable name="barcode">
			<xsl:value-of select="concat(format-number(ancestor::ProductionLot/@ProductionLot,'0000'),format-number($set,'00'),format-number($machine,'00'),format-number(@absoluteNumber,'0000'))"/>
		</xsl:variable>
		<xsl:variable name="SteelLength">
			<xsl:choose>
				<xsl:when test="Steels/Steel/@length &gt; 0">
					<xsl:value-of select="Steels/Steel/@length"></xsl:value-of>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="0"></xsl:value-of>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:variable name="SteelReference">
			<xsl:choose>
				<xsl:when test="Steels/Steel/@length &gt; 0">
					<xsl:value-of select="Steels/Steel/@reference"></xsl:value-of>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select=" '' "></xsl:value-of>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		
		<!-- Output -->
		<xsl:value-of select="$barCount"/>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="$RodNumber" />
		<xsl:text>;</xsl:text>
		<xsl:value-of select="$barLength" />
		<xsl:text>;</xsl:text>
		<xsl:value-of select="$pieceCount" />
		<xsl:text>;</xsl:text>
		<xsl:value-of select="format-number(@length * 10,'00000')" />
		<xsl:text>;</xsl:text>
		<xsl:value-of select="MachineSystem/@angleA" />
		<xsl:text>;</xsl:text>
		<xsl:choose>
			<xsl:when test="MachineSystem/@angleB = 45">
				<xsl:value-of select="MachineSystem/@angleB + 90"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="MachineSystem/@angleB"/>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="substring($barReference,4)" />
		<!--PROFILE STOCK CODE-->
		<xsl:text>;</xsl:text>
		<xsl:value-of select="ancestor::Rod/@Description" />
		<xsl:text>;</xsl:text>
		<!--<xsl:text>;</xsl:text>-->
		<xsl:value-of select="format-number($width,'0000')"></xsl:value-of>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="format-number($height,'0000')"></xsl:value-of>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="$trolley" />
		<xsl:text>;</xsl:text>
		<xsl:value-of select="$shelve" />
		<xsl:text>;</xsl:text>
		<xsl:value-of select="$position" />
		<xsl:text>;</xsl:text>
		<xsl:value-of select="$SteelReference"></xsl:value-of>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="floor($SteelLength)"></xsl:value-of>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="/ProductionLot/@ProductionLot"></xsl:value-of>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="concat(@number,'/',@version)"></xsl:value-of>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="@sortOrder"></xsl:value-of>
		<xsl:text>;</xsl:text>
		<xsl:text>MECAL</xsl:text>
		<!--MANUFACTURER NAME-->
		<xsl:text>;</xsl:text>
<!-- Campo musterit (Combinación: Modelo + Descripción Rod + Posición) -->
		<xsl:value-of select="@nomenclature"/>
		<xsl:text> </xsl:text>
		<xsl:value-of select="ancestor::Rod/@Description"/>
		<xsl:text> </xsl:text>
		<xsl:choose>
			<xsl:when test="@angle=90">
				<xsl:text>Derecha</xsl:text>
			</xsl:when>
			<xsl:when test="@angle=180">
				<xsl:text>Superior</xsl:text>
			</xsl:when>
			<xsl:when test="@angle=270">
				<xsl:text>Izquierda</xsl:text>
			</xsl:when>
			<xsl:when test="@angle=360">
				<xsl:text>Inferior</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="@angle"/>
			</xsl:otherwise>
		</xsl:choose>		<xsl:text>;</xsl:text>
		<xsl:value-of select="$colorCode"></xsl:value-of>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="$barColorName"></xsl:value-of>
		<xsl:text>;</xsl:text>
		<xsl:apply-templates select="descendant::Dimensions/Dimension[@name = 'L' or @name = 'A']"/>
		<xsl:text>;</xsl:text>
		<xsl:apply-templates select="descendant::Operations/Operation[MachineSystem/@X &gt; 0 and substring(@nameInMachine,1,1)='H']"/>
		<xsl:text>;</xsl:text>
		<xsl:value-of select="$barcode" />
		<xsl:text>;</xsl:text>
		<xsl:value-of select="concat($barReference, '.JPG')" />
		<xsl:value-of select="$breakLine" />
	</xsl:template>

	<xsl:template match="Operation">
		<xsl:value-of select="@nameInMachine"></xsl:value-of>
		<xsl:text>M</xsl:text>
		<xsl:choose>
			<xsl:when test="MachineSystem">
				<xsl:value-of select="format-number(MachineSystem/@X *10,'000000')"></xsl:value-of>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="format-number(@X *10,'000000')"></xsl:value-of>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="Dimension">
		<xsl:choose>
			<xsl:when test="@name = 'L'">
				<xsl:value-of select="@value"/>
				<!--WINDOW WIDTH-->
				<xsl:text>;</xsl:text>
			</xsl:when>
			<xsl:when test="@name = 'A'">
				<xsl:value-of select="@value"/>
				<!--WINDOW HEIGHT-->
			</xsl:when>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="toUpper">
		<xsl:param name="text"/>
		<xsl:value-of
		  select="translate(
	      $text,
	      'abcdefghijklmnopqrstuvwxyz',
	      'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
	    )"/>
	</xsl:template>
</xsl:stylesheet>


