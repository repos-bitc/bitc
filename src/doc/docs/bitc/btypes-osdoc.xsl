<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE xsl:stylesheet
[
<!ENTITY nbsp "&#x00A0;">

<!-- Include all of the entities used by OSDoc, for ease of reference.
     Do this by including the same dbcentx module that OSDoc
     includes. -->
]>
<!--
 Transformer from things in the osdoc DTD to HTML.
-->

<xsl:stylesheet
  version ="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:doc="http://www.eros-os.org/DTD/osdoc/0.1/osdoc.dtd"
  xmlns:btypes="http://www.bitc-lang.org/DTD/bitc/0.1/bitc-types.dtd"
  xmlns:m="http://www.w3.org/1998/Math/MathML">

  <xsl:output method="xml"
    encoding="utf-8"
    doctype-system="http://www.coyotos.org/OSDoc/DTD/osdoc-0.1.dtd"
    doctype-public="-//EROS Group//DTD OSDoc XML V0.1//EN"
    indent="yes"/>

  <!-- Wrapper handling -->
  <xsl:template match="TYPE">
    <xsl:apply-templates mode="formula"/>
  </xsl:template>

  <xsl:template match="btypes:formula">
    <xsl:apply-templates mode="formula"/>
  </xsl:template>

  <xsl:template match="btypes:TYPE">
    <xsl:apply-templates mode="formula"/>
  </xsl:template>

<!-- ======================================================================
     ======================================================================
               Common Operations
     ======================================================================
     ====================================================================== -->

  <!-- Infinity -->
  <xsl:template match="infinity" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>infin;</xsl:text>
  </xsl:template>


<!-- ======================================================================
               Base Operations
     ====================================================================== -->

  <!-- operator -->
  <xsl:template match="operator" mode="formula">
    <xsl:choose>
      <xsl:when test="@name">
	<xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:when test="@symbol">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:value-of select="@symbol"/>
	<xsl:text>;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <!-- error -->
  <xsl:template match="error" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>bottom;</xsl:text>
  </xsl:template>


<!-- ======================================================================
               Logical Operations
     ====================================================================== -->

  <!-- eqOp -->
  <xsl:template match="eqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>equals;</xsl:text>
    <xsl:choose>
      <xsl:when test="@under='minzT'">
	<xsl:element name="sub">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	  <xsl:text>dtrif;</xsl:text>
	</xsl:element>
      </xsl:when>
      <xsl:when test="@under='minz'">
	<xsl:element name="sub">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	  <xsl:text>xdtri;</xsl:text>
	</xsl:element>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <!-- eq -->
  <xsl:template match="eq" mode="formula">
    <xsl:call-template name="print.infix.implied">
      <xsl:with-param name="print.infix.implied.op">equals;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- neqOp -->
  <xsl:template match="neqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>ne;</xsl:text>
  </xsl:template>
  <!-- neq -->
  <xsl:template match="neq" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">ne;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- equivOp -->
  <xsl:template match="equivOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>equiv;</xsl:text>
  </xsl:template>
  <!-- eq -->
  <xsl:template match="equiv" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">equiv;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- gg -->
  <xsl:template match="gg" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">Gt;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- ll -->
  <xsl:template match="ll" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">Lt;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- approxOp -->
  <xsl:template match="approxOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>ap;</xsl:text>
  </xsl:template>
  <!-- approx --> 
  <xsl:template match="approx" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">ap;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

<!-- ======================================================================
               Arithmetic Operations
     ====================================================================== -->

  <!-- plusOp -->
  <xsl:template match="plusOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>plus;</xsl:text>
  </xsl:template>
  <!-- plus -->
  <xsl:template match="plus" mode="formula">
    <xsl:call-template name="print.infix.implied">
      <xsl:with-param name="print.infix.implied.op">plus;</xsl:with-param> 
      <xsl:with-param name="print.infix.br">
	<xsl:value-of select="@br"/>	
      </xsl:with-param>
      <xsl:with-param name="print.infix.nosp">
	<xsl:value-of select="@nosp"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <!-- minusOp -->
  <xsl:template match="minusOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>minus;</xsl:text>
  </xsl:template>
  <!-- minus -->
  <xsl:template match="minus" mode="formula">
    <xsl:call-template name="print.infix.implied">
      <xsl:with-param name="print.infix.implied.op">minus;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>
  <!-- abs -->
  <xsl:template match="abs" mode="formula">
    <xsl:text>|</xsl:text>
    <xsl:apply-templates mode="formula"/>
    <xsl:text>|</xsl:text>
  </xsl:template>


<!-- ======================================================================
               Set Operations
     ====================================================================== -->

  <!-- empty -->
  <xsl:template match="empty" mode="formula">
    <xsl:text>{}</xsl:text>
  </xsl:template>
  <!-- Empty -->
  <xsl:template match="Empty" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>empty;</xsl:text>
  </xsl:template>
  <!-- Empty -->
  <xsl:template match="EmptySubst" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>lang;</xsl:text>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rang;</xsl:text>
  </xsl:template>
  
  <!-- set -->
  <xsl:template match="set" mode="formula">
    <xsl:call-template name="print.set"/>
  </xsl:template>  
  <!-- aSet -->
  <xsl:template match="aSet" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>chi;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  

  <!-- inOp -->
  <xsl:template match="inOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>in;</xsl:text>
  </xsl:template>
  <!-- in --> 
  <xsl:template match="in" mode="formula">
    <xsl:for-each select="*">
      <xsl:if test = "position() &gt; 1">
	<xsl:if test = "position() &lt; (last() - 1)">
	  <xsl:call-template name="print.op.comma"/>
	</xsl:if>
      </xsl:if>
      <xsl:if test = "position() = last()">
	<xsl:call-template name="print.op.in"/>	
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>
  
  <!-- notinOp -->
  <xsl:template match="notinOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>notin;</xsl:text>
  </xsl:template>
  <!-- notin --> 
  <xsl:template match="notin" mode="formula">
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() &lt; (last() - 1)">
	<xsl:call-template name="print.op.comma"/>
      </xsl:if>
      <xsl:if test = "position() =  last() - 1">
	<xsl:call-template name="print.op.notin"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
 
  <!-- supOp -->
  <xsl:template match="supOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sup;</xsl:text>
  </xsl:template>
  <!-- sup -->
  <xsl:template match="sup" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">sup;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- supeqOp -->
  <xsl:template match="supeqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>supe;</xsl:text>
  </xsl:template>
  <!-- supeq -->
  <xsl:template match="supeq" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">supe;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>
  
  <!-- subOp -->
  <xsl:template match="subOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sub;</xsl:text>
  </xsl:template>
  <!-- sub -->
  <xsl:template match="sub" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">sub;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>
  
  <!-- subeqOp -->
  <xsl:template match="subeqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sube;</xsl:text>
  </xsl:template>
  <!-- subeq -->
  <xsl:template match="subeq" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">sube;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- uninOp -->
  <xsl:template match="uninOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>cup;</xsl:text>
  </xsl:template>
  <!-- unin -->
  <xsl:template match="unin" mode="formula">
    <xsl:call-template name="print.infix.implied">
      <xsl:with-param name="print.infix.implied.op">cup;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template> 
  <!-- UninOp -->
  <xsl:template match="UninOp" mode="formula">
    <xsl:element name="b">
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>cup;</xsl:text>
    </xsl:element>
  </xsl:template>
  <!-- Unin -->
  <xsl:template match="Unin" mode="formula">
    <xsl:element name="b">
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>cup;</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.space"/>
    <xsl:apply-templates mode="formula"/>
  </xsl:template> 

  <!-- interOp -->
  <xsl:template match="interOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>cap;</xsl:text>
  </xsl:template>
  <!-- inter -->
  <xsl:template match="inter" mode="formula">
    <xsl:call-template name="print.infix.implied">
      <xsl:with-param name="print.infix.implied.op">cap;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  
  <!-- InterOp -->
  <xsl:template match="InterOp" mode="formula">
    <xsl:element name="b">
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>cap;</xsl:text>
    </xsl:element>
  </xsl:template>
  <!-- Inter -->
  <xsl:template match="Inter" mode="formula">
    <xsl:element name="b">
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>cap;</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.space"/>
    <xsl:apply-templates mode="formula"/>
  </xsl:template>  

  <!-- diffOp -->
  <xsl:template match="diffOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>setmn;</xsl:text>
  </xsl:template>
  <!-- diff -->
  <xsl:template match="diff" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">setmn;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- mutexOp -->
  <xsl:template match="mutexOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>osol;</xsl:text>
  </xsl:template>
  <!-- mutex -->
  <xsl:template match="mutex" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">osol;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  


  <!-- map -->
  <xsl:template match="map" mode="formula">
    <xsl:element name="progident">
      <xsl:text>map</xsl:text>      
    </xsl:element>    
    <xsl:call-template name="print.params"/>
  </xsl:template>  

  <!-- dom --> 
  <xsl:template match="dom" mode="formula">
    <xsl:element name="em">
      <xsl:text>dom</xsl:text>      
    </xsl:element>    
    <xsl:call-template name="print.params"/>
  </xsl:template>

  <!-- range --> 
  <xsl:template match="range" mode="formula">
    <xsl:element name="em">
      <xsl:text>range</xsl:text>      
    </xsl:element>    
    <xsl:call-template name="print.params"/>
  </xsl:template>
  
  <!-- mapsto --> 
  <xsl:template match="mapsto" mode="formula">
    <xsl:for-each select="*">
      <xsl:if test = "position() = 1">
	<xsl:apply-templates select="." mode="formula"/>	
      </xsl:if>
      <xsl:if test = "position() = 2">
  	<xsl:text>(</xsl:text>    	
	<xsl:apply-templates select="." mode="formula"/>	
  	<xsl:text>)</xsl:text>    
      </xsl:if>
      <xsl:if test = "position() = 3">
	<xsl:call-template name="print.op.eq"/>
	<xsl:apply-templates select="." mode="formula"/>	
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

<!-- ======================================================================
               Miscellaneous  Operations
     ====================================================================== -->
  <!-- asOp -->
  <xsl:template match="asOp" mode="formula">
    <xsl:element name="progident">
      <xsl:text>as</xsl:text>      
    </xsl:element>    
  </xsl:template>  
  <!-- as -->
  <xsl:template match="as" mode="formula">
    <xsl:call-template name="print.infix.kwd">
      <xsl:with-param name="print.infix.kwd.kwd">as</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- pluaral ...* -->
  <xsl:template match="plural" mode="formula">
    <!-- <xsl:text>(</xsl:text> -->
    <xsl:apply-templates mode="formula"/>
    <!-- <xsl:text>)</xsl:text> -->
    <xsl:call-template name="print.plural"/>
  </xsl:template>

  <!-- Paranthesized expression -->
  <xsl:template match="paren" mode="formula">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates mode="formula"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <!-- Double Bracket expression -->
  <xsl:template match="DBrac" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>#x301A;</xsl:text>
    <xsl:apply-templates mode="formula"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>#x301B;</xsl:text>
  </xsl:template>

  <!-- Special Set -->
  <xsl:template match="spset" mode="formula">
    <xsl:call-template name="print.spset"/>
  </xsl:template>
  
  <!-- mappping -->
  <xsl:template match="mapping" mode="formula">
    <xsl:for-each select="*">
      <xsl:if test = "position() = last()">	
	<xsl:call-template name="print.op.map"/>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>  

  <!-- fnxn -->
  <xsl:template match="fnxn" mode="formula">
    <xsl:call-template name="print.fnxn"/> 
  </xsl:template>  
  
  <!-- Transformations -->
  <xsl:template match="Xform" mode="formula">
    <xsl:if test="@isEntity = 'yes'">
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    </xsl:if>
    <xsl:value-of select="@name"/>
    <xsl:if test="@isEntity = 'yes'">
      <xsl:text>;</xsl:text>
    </xsl:if>
    <xsl:call-template name="print.params"/>
  </xsl:template> 

  <!-- Infix operator -->
  <xsl:template match="op" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">
	<xsl:value-of select="@symbol"/>
	<xsl:text>;</xsl:text>
      </xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  
 
  <!-- leadsto -->
  <xsl:template match="leadsto" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">rarrw;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- TransClose -->
  <xsl:template match="TransClose" mode="formula">
    <xsl:apply-templates mode="formula"/>
    <xsl:element name="sup">
      <xsl:element name="sup">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>plus;</xsl:text>
      </xsl:element>
    </xsl:element>
  </xsl:template>  
  
  <!-- floorOp -->
  <xsl:template match="floorOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>lfloor;</xsl:text>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rfloor;</xsl:text>
  </xsl:template>
  <!-- floor -->
  <xsl:template match="floor" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>lfloor;</xsl:text>
    <xsl:apply-templates mode="formula"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rfloor;</xsl:text>
  </xsl:template>

  <!-- ceilOp -->
  <xsl:template match="ceilOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>lceil;</xsl:text>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rceil;</xsl:text>
  </xsl:template>
  <!-- ceil -->
  <xsl:template match="ceil" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>lceil;</xsl:text>
    <xsl:apply-templates mode="formula"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rceil;</xsl:text>
  </xsl:template>

  <!-- incMBOp -->
  <xsl:template match="incMBOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>curwed;</xsl:text>
  </xsl:template>
  <!-- incrMB -->
  <xsl:template match="incMB" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>curwed;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>  

  <!-- incrOp -->
  <xsl:template match="incrOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>utri;</xsl:text>
  </xsl:template>
  <!-- incr -->
  <xsl:template match="incr" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>utri;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>  

  <!-- decrOp -->
  <xsl:template match="decrOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>dtri;</xsl:text>
  </xsl:template>
  <!-- decr -->
  <xsl:template match="decr" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>dtri;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>  

  <!-- maxzOp -->
  <xsl:template match="maxzOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>xutri;</xsl:text>
  </xsl:template>
  <!-- maxz -->
  <xsl:template match="maxz" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>xutri;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>  
  
  <!-- minzOp -->
  <xsl:template match="minzOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>xdtri;</xsl:text>
  </xsl:template>
  <!-- minz -->
  <xsl:template match="minz" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>xdtri;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>

  <!-- maxzTOp -->
  <xsl:template match="maxzTOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>utrif;</xsl:text>
  </xsl:template>
  <!-- maxzT -->
  <xsl:template match="maxzT" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>utrif;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>  

  <!-- minzTOp -->
  <xsl:template match="minzTOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>dtrif;</xsl:text>
  </xsl:template>
  <!-- minzT -->
  <xsl:template match="minzT" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>dtrif;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>

  <!-- minzDOp -->
  <xsl:template match="minzDOp" mode="formula">
    <xsl:call-template name="print.mathfrac">
      <xsl:with-param name="print.mathfrac.letter">I</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>
  <!-- minzD -->
  <xsl:template match="minzD" mode="formula">
    <xsl:call-template name="print.mathfrac">
      <xsl:with-param name="print.mathfrac.letter">I</xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.params"/>
  </xsl:template>

  <!-- concrete -->
  <xsl:template match="concrete" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>square;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>
  
  <!-- spOp -->
  <xsl:template match="spOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sqsub;</xsl:text>
  </xsl:template>
  <!-- sp -->
  <xsl:template match="sp" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">sqsub;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- spEqOp -->
  <xsl:template match="spEqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sqsube;</xsl:text>
  </xsl:template>
  <!-- spEq -->
  <xsl:template match="spEq" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">sqsube;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- genOp -->
  <xsl:template match="genOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sqsup;</xsl:text>
  </xsl:template>
  <!-- gen -->
  <xsl:template match="gen" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">sqsup;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- genEqOp -->
  <xsl:template match="genEqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sqsupe;</xsl:text>
  </xsl:template>
  <!-- genEq -->
  <xsl:template match="genEq" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">sqsupe;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- Canonical Forms -->
  <xsl:template match="canonical" mode="formula">
    <xsl:element name="u">
      <xsl:apply-templates mode="formula"/>
    </xsl:element>
  </xsl:template>

  <!-- Normalize -->
  <xsl:template match="normalize" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Ngr;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>

  <!-- relevant -->
  <xsl:template match="relevant" mode="formula">
    <xsl:text>|</xsl:text>
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:text>|</xsl:text>
    <xsl:element name="sup">
      <xsl:apply-templates select="*[2]" mode="formula"/>
    </xsl:element>
  </xsl:template>  

  
<!-- ======================================================================
              Substitutions
     ====================================================================== -->
  <xsl:template match="aSubMap" mode="formula">
    <!--     <xsl:text disable-output-escaping="yes">&amp;</xsl:text> -->
    <!--     <xsl:text>thetas;</xsl:text> -->
    <xsl:text>S</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <xsl:template match="SubMap" mode="formula">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.op.sub"/>
    <xsl:apply-templates select="*[2]" mode="formula"/>    
    <xsl:text>]</xsl:text>
  </xsl:template>

  <xsl:template match="compose" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">compfn;</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="scomp" mode="formula">
    <xsl:if test="count(*) &lt; 1">
      <xsl:text>*** ERROR: At least one substitution required for scomp ***</xsl:text>  
    </xsl:if>
    <xsl:element name="em">
      <xsl:text>S</xsl:text>
      <xsl:element name="sub">
	<xsl:for-each select="*">
	  <xsl:if test = ". != aSubMap">
	    <xsl:text>*** ERROR: Only SubMaps allowed in scomp ***</xsl:text>
	  </xsl:if>
	  <xsl:choose>
	    <xsl:when test = "@num">
	      <xsl:if test = "position() &gt; 1">
		<xsl:text>.</xsl:text>
	      </xsl:if>
	      <xsl:value-of select="@num"/>
	      <xsl:if test="@subnum">
		<xsl:element name="sub">
		  <xsl:value-of select="@subnum"/>
		</xsl:element>
	      </xsl:if>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>*** ERROR: scomp without subscript not allowed ***</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:if test = "@dash">
	    <xsl:call-template name="print.primed_text">
	      <xsl:with-param name="print.primed_text.text">
		<xsl:value-of select="@dash"/>
	      </xsl:with-param>
	    </xsl:call-template>
	    <xsl:text>*** ERROR: scomp with prime not allowed ***</xsl:text>
	  </xsl:if>
	</xsl:for-each>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="subst" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:text>[</xsl:text>	  
    <xsl:apply-templates select="*[3]" mode="formula"/>	
    <xsl:text>/</xsl:text>	  
    <xsl:apply-templates select="*[2]" mode="formula"/>	
    <xsl:text>]</xsl:text>	  
  </xsl:template>  

  <xsl:template match="Csubst" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:element name="b">
      <xsl:element name="em">
	<xsl:text>[</xsl:text>	  
      </xsl:element>
    </xsl:element>
    <xsl:apply-templates select="*[3]" mode="formula"/>	
    <xsl:element name="b">
      <xsl:element name="em">
	<xsl:text>/</xsl:text>	  
      </xsl:element>
    </xsl:element>
    <xsl:apply-templates select="*[2]" mode="formula"/>	
    <xsl:element name="b">
      <xsl:element name="em">
	<xsl:text>]</xsl:text>	  
      </xsl:element>
    </xsl:element>
  </xsl:template>    
  
  <xsl:template match="Subst" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:choose>
      <xsl:when test="*[2] = set">
      </xsl:when>
      <xsl:when test="*[2] = spset">
      </xsl:when>
      <xsl:otherwise>
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>lang;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="*[2]" mode="formula"/>	
    <xsl:choose>
      <xsl:when test="*[2] = set">
      </xsl:when>
      <xsl:when test="*[2] = spset">
      </xsl:when>
      <xsl:otherwise>
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>rang;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>  

<!-- ======================================================================
              Tape
     ====================================================================== -->
  
  <xsl:template match="tape" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Pi;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <xsl:template match="roll" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:element name="sub">
      <xsl:for-each select="*[position() &gt; 1]">
	<xsl:apply-templates select="." mode="formula"/>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>  

<!-- ======================================================================
               Generic Collections
     ====================================================================== -->

  <!-- grouping --> 
  <xsl:template match="grouping" mode="formula">
    <xsl:apply-templates mode="formula"/>
  </xsl:template>
      
  <!-- collection --> 
  <xsl:template match="collection" mode="formula">
    <xsl:if test="@paren = 'yes'">
      <xsl:text>(</xsl:text>
    </xsl:if>
    <xsl:for-each select="*">
      <xsl:choose>
	<xsl:when test="position() = 1"/>
	<xsl:when test="position() = last()">	
	  <xsl:choose>
	    <xsl:when test="../@sep">
	      <xsl:value-of select="../@sep"/> 
	      <xsl:text> </xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>, </xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:if test="../@and">
	    <xsl:text> and </xsl:text>
	  </xsl:if>
	  <xsl:if test="../@or">
	    <xsl:text> or </xsl:text>
	  </xsl:if>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:choose>
	    <xsl:when test="../@sep">
	      <xsl:value-of select="../@sep"/> 
	      <xsl:text> </xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>, </xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
    <xsl:if test="@etc = 'yes'">
      <xsl:text>, ...</xsl:text>
    </xsl:if>
    <xsl:if test="@paren = 'yes'">
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- alternatives --> 
  <xsl:template match="alternatives" mode="formula">
    <xsl:call-template name="print.alts"/>
    <xsl:if test="@etc = 'yes'">
      <xsl:text> | ...</xsl:text>
    </xsl:if>
  </xsl:template>

<!-- ======================================================================
                              Predicates
     ====================================================================== -->

  <!--LOGIC -->
  <xsl:template match="LOGIC" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Omega;</xsl:text>
    <xsl:call-template name="print.index.dash"/>    
  </xsl:template>

  <!-- TRUE -->
  <xsl:template match="TRUE" mode="formula">
    <xsl:element name="b">
      <xsl:text>true</xsl:text>
    </xsl:element>
  </xsl:template>
  
  <!-- FALSE -->
  <xsl:template match="FALSE" mode="formula">
    <xsl:element name="b">
      <xsl:text>false</xsl:text>
    </xsl:element>
  </xsl:template>

  <!-- AND -->
  <xsl:template match="AND" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">and;</xsl:with-param> 
      <xsl:with-param name="print.infix.br">
	<xsl:value-of select="@br"/>	
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- OR -->
  <xsl:template match="OR" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">or;</xsl:with-param> 
      <xsl:with-param name="print.infix.br">
	<xsl:value-of select="@br"/>	
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <!-- NOT -->
  <xsl:template match="NOT" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>not;</xsl:text>
    <xsl:apply-templates mode="formula"/>
  </xsl:template>

  <!-- IMPLIES -->
  <xsl:template match="IMPLIES" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">rArr;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>
  
  <!-- forall -->
  <xsl:template match="forall" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>forall;</xsl:text>  
    <xsl:call-template name="print.quant"/>  
  </xsl:template>
  <!-- exists -->
  <xsl:template match="exists" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>exist;</xsl:text>    
    <xsl:call-template name="print.quant"/>  
  </xsl:template>
  <!-- Forall -->
  <xsl:template match="Forall" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>forall;</xsl:text>  
    <xsl:call-template name="print.space"/>	  
    <xsl:call-template name="print.children.nbsp"/>  
    <xsl:call-template name="print.space"/>	  
  </xsl:template>
  <!-- Exists -->
  <xsl:template match="Exists" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>exist;</xsl:text>    
    <xsl:call-template name="print.space"/>	  
    <xsl:call-template name="print.children.nbsp"/>  
    <xsl:call-template name="print.space"/>	  
  </xsl:template>

  <!-- iff -->
  <xsl:template match="iff" mode="formula">
    <xsl:call-template name="print.infix.it">
      <xsl:with-param name="print.infix.it.kwd">iff</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- pred -->
  <xsl:template match="pred" mode="formula">
    <xsl:call-template name="print.mathmode">
      <xsl:with-param name="print.mathmode.text">
	<xsl:value-of select="@name"/>
      </xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.index.dash"/>
    <xsl:call-template name="print.params"/>
  </xsl:template>

  <!-- constrained entities -->
  <xsl:template match="constrained" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">\</xsl:with-param> 
      <xsl:with-param name="print.infix.noamp">yes</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  
  
  <!-- TypEvalPred -->
  <xsl:template match="TypEvalPred" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>weierp;</xsl:text>
    <xsl:text>(</xsl:text>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rArr;</xsl:text>    
    <xsl:call-template name="print.op.comma"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>vdash;</xsl:text> 
    <xsl:text>)</xsl:text>    
  </xsl:template>
  
  <!-- solvable -->
  <xsl:template match="solvable" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>omega;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- solvables -->
  <xsl:template match="solvables" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>omega;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
    <xsl:call-template name="print.plural"/>
  </xsl:template>

<!-- ======================================================================
                        Text  
     ====================================================================== -->
  <!-- text -->
  <xsl:template match="text" mode="formula">
    <xsl:value-of select="@content"/>
  </xsl:template>

  <!-- texttt -->
  <xsl:template match="texttt" mode="formula">
    <xsl:element name="progident">
      <xsl:value-of select="@content"/>
    </xsl:element>
  </xsl:template>

  <!-- textit -->
  <xsl:template match="textit" mode="formula">
    <xsl:element name="em">
      <xsl:value-of select="@content"/>
    </xsl:element>
  </xsl:template>

  <!-- textbf -->
  <xsl:template match="textbf" mode="formula">
    <xsl:element name="b">
      <xsl:value-of select="@content"/>
    </xsl:element>
  </xsl:template>

  <!-- textu -->
  <xsl:template match="textu" mode="formula">
    <xsl:element name="u">
      <xsl:value-of select="@content"/>
    </xsl:element>
  </xsl:template>

  <!-- textmath -->
  <xsl:template match="textmath" mode="formula">
    <xsl:call-template name="print.mathmode">
      <xsl:with-param name="print.mathmode.text">
	<xsl:value-of select="@content"/>
      </xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- space -->
  <xsl:template match="space" mode="formula">
    <xsl:call-template name="print.spaces">
      <xsl:with-param name="print.spaces.n">
	<xsl:choose>
	  <xsl:when test="@n">
	    <xsl:value-of select="@n"/>
	  </xsl:when>
	  <xsl:otherwise>
	    1
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- nothing -->
  <xsl:template match="nothing" mode="formula">
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- unknown -->
  <xsl:template match="unknown" mode="formula">
    <xsl:text>??</xsl:text>
  </xsl:template>

  <!-- unspecified -->
  <xsl:template match="unspecified" mode="formula">
    <!--     <xsl:text disable-output-escaping="yes">&amp;</xsl:text> -->
    <!--     <xsl:text>mldr;</xsl:text> -->
    <xsl:text>...</xsl:text>
  </xsl:template>
  
  <!-- symbol -->
  <xsl:template match="symbol" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

 <!-- ======================================================================
      ======================================================================
                                TYPES
      ======================================================================
      ====================================================================== -->
 
  <!--  ====================================================================
                         Core Types
        ==================================================================== -->

  <!-- ANY type -->
  <xsl:template match="type" mode="formula">
    <xsl:call-template name="print.type"/>
  </xsl:template>

  <!-- Special type syntax -->
  <xsl:template match="stype" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sigmav;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  
  <xsl:template match="ptype" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rho;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  
  <xsl:template match="Ptype" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rhov;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  
  
  <!-- Type Variables -->
  <xsl:template match="tvar" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:choose>
      <xsl:when test = "@name">
	<xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>alpha</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>
  
  <!-- Type Variable set -->
  <xsl:template match="aTVset" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>chi;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- Dummy types -->
  <xsl:template match="dummy" mode="formula">
    <xsl:text>#X</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- Primitive Types -->
  <xsl:template match="unit" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>unit</xsl:text>
    </xsl:element>
  </xsl:template>

  <xsl:template match="char" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>char</xsl:text>
    </xsl:element>
  </xsl:template>

  <xsl:template match="string" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>string</xsl:text>
    </xsl:element>
  </xsl:template>

  <xsl:template match="bool" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>bool</xsl:text>
    </xsl:element>
  </xsl:template>

  <xsl:template match="word" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>word</xsl:text>
    </xsl:element>
  </xsl:template>

  <xsl:template match="int" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>int</xsl:text>
      <xsl:value-of select="@sz"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="uint" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>uint</xsl:text>
      <xsl:value-of select="@sz"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="float" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>float</xsl:text>
    </xsl:element>
  </xsl:template>

  <xsl:template match="quad" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>quad</xsl:text>
    </xsl:element>
  </xsl:template>

  <xsl:template match="double" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>double</xsl:text>
    </xsl:element>
  </xsl:template>

  <!-- Functions ... &rarr; ... -->
  <xsl:template match="fnOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rarr;</xsl:text>
  </xsl:template>
  <xsl:template match="fn" mode="formula">
    <xsl:if test = "@M">	
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>lfloor;</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:if test = "@M">	
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>rfloor;</xsl:text>
    </xsl:if>
    <xsl:call-template name="print.space"/>    
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rarr;</xsl:text>
    <xsl:call-template name="print.space"/>    
    <xsl:if test = "@M">	
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>lceil;</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="*[2]" mode="formula"/>
    <xsl:if test = "@M">	
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>rceil;</xsl:text>
    </xsl:if>
  </xsl:template>
  
  <!-- ref -->
  <xsl:template match="refOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>uArr;</xsl:text>
  </xsl:template>
  <xsl:template match="ref" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>uArr;</xsl:text>
    <xsl:apply-templates mode="formula"/>
  </xsl:template>
  
  <!-- pair -->
  <xsl:template match="pair" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.op.times"/>
    <xsl:apply-templates select="*[2]" mode="formula"/>
  </xsl:template>

  <!-- list -->
  <xsl:template match="list" mode="formula">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:text>]</xsl:text>
  </xsl:template>

  <!-- tuple -->
  <xsl:template match="tuple" mode="formula">
    <xsl:call-template name="print.params_paren"/>
  </xsl:template>
  
  <!-- array  -->
  <xsl:template match="array" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>arr</xsl:text>
    </xsl:element>    
    <xsl:if test="@len">      
      <xsl:element name="sup">    
	<xsl:value-of select="@len"/>
      </xsl:element>
    </xsl:if>
    <xsl:call-template name="print.params"/>
  </xsl:template>

  <!-- vector -->
  <xsl:template match="vector" mode="formula">
    <xsl:element name="progident">    
      <xsl:text>vec</xsl:text>
    </xsl:element>    
    <xsl:call-template name="print.params"/>
  </xsl:template>

  <!-- mutable &Psi; ... -->
  <xsl:template match="mutableOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Psi;</xsl:text>
  </xsl:template>
  <xsl:template match="mutable" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Psi;</xsl:text>
    <xsl:if test = "(node() = fn) or (node() = pair)">
      <xsl:text>(</xsl:text>
    </xsl:if>
    <xsl:apply-templates mode="formula"/>
    <xsl:if test = "(node() = fn) or (node() = pair)">
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- maybe &Upsi -->
  <xsl:template match="maybeOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Upsi;</xsl:text>
  </xsl:template>
  <xsl:template match="maybe" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Upsi;</xsl:text>
    <xsl:apply-templates mode="formula"/>
  </xsl:template>

  <!-- Exception &xi; ... -->
  <xsl:template match="exception" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>xi;</xsl:text>
    <xsl:apply-templates mode="formula"/>
  </xsl:template>

  <!-- structures -->
  <xsl:template match="struct" mode="formula">
    <xsl:choose>
      <xsl:when test="@inline">
	<xsl:value-of select="@name"/>
	<xsl:call-template name="print.params"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="print.comp">
	  <xsl:with-param name="print.comp.symbol">otimes;</xsl:with-param> 
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- unions -->
  <xsl:template match="union" mode="formula">
    <xsl:choose>
      <xsl:when test="@inline">
	<xsl:value-of select="@name"/>
	<xsl:call-template name="print.params"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="print.comp">
	  <xsl:with-param name="print.comp.symbol">oplus;</xsl:with-param> 
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <!-- structures or unions -->
  <xsl:template match="comp" mode="formula">
    <xsl:call-template name="print.comp">
      <xsl:with-param name="print.comp.symbol">ominus;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- constrained type -->
  <xsl:template match="ctype" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">\</xsl:with-param> 
      <xsl:with-param name="print.infix.noamp">yes</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

 <!-- A constrained type -->
  <xsl:template match="aCtype" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rho;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  
  
  <!-- constrained type, Typeclass Notation -->
  <xsl:template match="CType" mode="formula">
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() &lt; (last()-1)">
	<xsl:call-template name="print.op.comma"/>
      </xsl:if>
      <xsl:if test = "position() = (last()-1)">
	<xsl:call-template name="print.op.implies"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>  

  <!-- mbTop -->
  <xsl:template match="mbTop" mode="formula">
    <xsl:call-template name="print.maybe"/>
  </xsl:template>    

  <!-- mbFull -->
  <xsl:template match="mbFull" mode="formula">
    <xsl:call-template name="print.maybe">
      <xsl:with-param name="print.maybe.double">yes</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- mbpair -->
  <xsl:template match="mbpair" mode="formula">
    <xsl:call-template name="print.maybe">
      <xsl:with-param name="print.maybe.double">yes</xsl:with-param>
    </xsl:call-template>
  </xsl:template>    
  
  <!-- MBpair -->
  <xsl:template match="MBpair" mode="formula">
    <xsl:text>[[</xsl:text>
    <xsl:call-template name="print.children"/>
    <xsl:text>]]</xsl:text>
  </xsl:template>    
  
  <!-- Maybe hints (aHset) -->
  <xsl:template match="aHset" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>planck;</xsl:text>
    <xsl:call-template name="print.index.dash"/>    
  </xsl:template>  
  
  <!-- inner: Inner type of the maybe types -->
  <xsl:template match="inner" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>image;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>  

  <!-- join and meet -->
  <xsl:template match="join" mode="formula">
    <xsl:call-template name="print.procedure">
      <xsl:with-param name="print.procedure.name">join</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  
  <xsl:template match="meet" mode="formula">
    <xsl:call-template name="print.procedure">
      <xsl:with-param name="print.procedure.name">meet</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- Qualified Type -->
  <xsl:template match="Qtype" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:if test = "node() = fn">
      <xsl:text>(</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="*[2]" mode="formula"/>	
    <xsl:if test = "node() = fn">
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- Qualifications -->
  <xsl:template match="aQual" mode="formula">
    <xsl:text>q</xsl:text>    
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>
  <xsl:template match="Qglobal" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>square;</xsl:text>
  </xsl:template>
  <xsl:template match="Qlocal" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>phis;</xsl:text>
  </xsl:template>
  
  <!--  ====================================================================
              Notational Types
        ==================================================================== -->
  <!-- aggregate ...* -->
  <xsl:template match="aggregate" mode="formula">
    <!-- <xsl:text>(</xsl:text> -->    
    <xsl:apply-templates mode="formula"/>
    <!-- <xsl:text>)</xsl:text> -->    
    <xsl:call-template name="print.plural"/>
  </xsl:template>

  <!-- Meta Syntax -->
  <xsl:template match="meta" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>real;</xsl:text>
    <xsl:call-template name="print.index.dash"/>    
    <xsl:call-template name="print.params"/>
  </xsl:template>  

<!--  ====================================================================
                  Constarints
      ==================================================================== -->
  <xsl:template match="Tclass" mode="formula">
    <xsl:value-of select="@name"/>
    <xsl:call-template name="print.params"/>
  </xsl:template>  

  <!-- constraint -->
  <xsl:template match="constraint" mode="formula">
    <xsl:call-template name="print.mathmode">
      <xsl:with-param name="print.mathmode.text">c</xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.index.dash"/>    
  </xsl:template>  

  <!-- Polymorphic constraint -->
  <!-- PcstOp -->
  <xsl:template match="PcstOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rarrhk;</xsl:text>	
    <xsl:element name="sup">
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>kappa;</xsl:text>
    </xsl:element>
    <xsl:apply-templates select="*[3]" mode="formula"/>
  </xsl:template>  
  <!-- Pcst -->
  <xsl:template match="Pcst" mode="formula">
    <xsl:apply-templates select="*[2]" mode="formula"/>
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rarrhk;</xsl:text>	
    <xsl:element name="sup">
      <xsl:apply-templates select="*[1]" mode="formula"/>
    </xsl:element>
    <xsl:call-template name="print.space"/>
    <xsl:apply-templates select="*[3]" mode="formula"/>
  </xsl:template>  

  <!-- unct -->
  <xsl:template match="unct" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:element name="sup">
      <xsl:text>.</xsl:text>
<!--       <xsl:text disable-output-escaping="yes">&amp;</xsl:text> -->
<!--       <xsl:text>compfn;</xsl:text> -->
    </xsl:element>
  </xsl:template>  
  
  <!-- ct_set templates -->
  <xsl:template match="aCtset" mode="formula">
    <xsl:call-template name="print.mathcal">
      <xsl:with-param name="print.mathcal.letter">C</xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.index.dash"/>    
  </xsl:template>  

  <!-- ct_set templates type 2 -->
  <xsl:template match="bCtset" mode="formula">
    <xsl:call-template name="print.mathmode">
      <xsl:with-param name="print.mathmode.text">C</xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.index.dash"/>    
  </xsl:template>  
  
  <!--  ====================================================================
                     TypeScheme
        ==================================================================== -->
  <!-- aTS: Type scheme -->
  <xsl:template match="aTS" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sigma;</xsl:text>
    <xsl:call-template name="print.index.dash"/>    
  </xsl:template>
  
  <!-- sTS: Special Type Scheme -->
  <xsl:template match="sTS" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rhov;</xsl:text>
    <xsl:call-template name="print.index.dash"/>    
  </xsl:template>

  <xsl:template match="TS" mode="formula">
    <xsl:if test="count(*) &gt; 1">
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>forall;</xsl:text>  
    </xsl:if>
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() &lt; (last()-1)">
	<xsl:call-template name="print.op.comma"/>
      </xsl:if>
      <xsl:if test = "position() = (last()-1)">
	<xsl:call-template name="print.space"/>
	<xsl:text>.</xsl:text>    
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  
<!-- ======================================================================
                                   Type classes 
     ====================================================================== -->
  <!-- Typeclass -->
  <xsl:template match="typeclass" mode="formula">
    <xsl:value-of select="@name"/>
    <xsl:call-template name="print.params"/>
  </xsl:template>

  <!-- Functional Dependencies -->
  <xsl:template match="tyfn" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">rarrw;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

<!-- ======================================================================
               Type only operations
     ====================================================================== -->
  <!-- ceqOp -->
  <xsl:template match="ceqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>cong;</xsl:text>
  </xsl:template>
  <!-- ceq -->
  <xsl:template match="ceq" mode="formula">
    <xsl:choose>
      <xsl:when test="@via">
	<xsl:call-template name="print.infix">
	  <xsl:with-param name="print.infix.op">cong;</xsl:with-param> 
	  <xsl:with-param name="print.infix.sub">
	    <xsl:value-of select="@via"/>
	    <xsl:text>;</xsl:text>
	  </xsl:with-param>
	</xsl:call-template>   
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="print.infix">
	  <xsl:with-param name="print.infix.op">cong;</xsl:with-param> 
	</xsl:call-template>    
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- nceqOp -->
  <xsl:template match="nceqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>ncong;</xsl:text>
  </xsl:template>
  <!-- nceq -->
  <xsl:template match="nceq" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">ncong;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- TceqOp -->
  <xsl:template match="TceqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>sime;</xsl:text>
  </xsl:template>
  <!-- Tceq -->
  <xsl:template match="Tceq" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">sime;</xsl:with-param> 
    </xsl:call-template>   
  </xsl:template>

  <!-- TnceqOp -->
  <xsl:template match="nceqOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>nsime;</xsl:text>
  </xsl:template>
  <!-- Tnceq -->
  <xsl:template match="nceq" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">nsime;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- TsubOp -->
  <xsl:template match="TsubOp" mode="formula">
    <xsl:call-template name="print.op.subqual"/>
  </xsl:template>
  <!-- Tsub -->
  <xsl:template match="Tsub" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">cupre;:</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- tsubOp -->
  <xsl:template match="tsubOp" mode="formula">
    <xsl:call-template name="print.op.subQual"/>
  </xsl:template>
  <!-- tsub -->
  <xsl:template match="tsub" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">le;:</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- MsubOp -->
  <xsl:template match="MsubOp" mode="formula">
    <xsl:call-template name="print.op.Mqual"/>
  </xsl:template>
  <!-- Msub -->
  <xsl:template match="Msub" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">ltrie;:</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- MadjOp -->
  <xsl:template match="MadjOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>weierp;</xsl:text>
  </xsl:template> 
  
  <!-- corUpOp -->
  <xsl:template match="coerceOp" mode="formula">
    <xsl:call-template name="print.op.corUp"/>
  </xsl:template>
  <!-- corUpOp -->
  <xsl:template match="coerceOp" mode="formula">
    <xsl:call-template name="print.op.corDown"/>
  </xsl:template>
 
<!-- ======================================================================
              Unification     
     ====================================================================== -->
  <!-- lnkOp -->
  <xsl:template match="lnkOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rarrhk;</xsl:text>
 </xsl:template>
  
  <!-- lnk -->
  <xsl:template match="lnk" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">rarrhk;</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- ln_set templates -->
  <xsl:template match="aLnset" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>lagran;</xsl:text>
    <xsl:call-template name="print.index.dash"/>    
  </xsl:template>  
  
  <!-- TheType -->
  <xsl:template match="TheType" mode="formula">
    <xsl:call-template name="print.procedure">
      <xsl:with-param name="print.procedure.name">TheType</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- unify -->
  <xsl:template match="unify" mode="formula">
    <xsl:call-template name="print.procedure">
      <xsl:with-param name="print.procedure.name">Unify</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- UNIFY -->
  <xsl:template match="UNIFY" mode="formula">
    <xsl:apply-templates select="*[3]" mode="formula"/>	
    <xsl:call-template name="print.space"/>
    <xsl:call-template name="print.derives">
      <xsl:with-param name="print.derives.name">unf</xsl:with-param> 
    </xsl:call-template>
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:call-template name="print.op.eq"/>
    <xsl:apply-templates select="*[2]" mode="formula"/>	
  </xsl:template>

  <!-- unf -->
  <xsl:template match="unf" mode="formula">
    <xsl:call-template name="print.mathcal">
      <xsl:with-param name="print.mathcal.letter">U</xsl:with-param> 
    </xsl:call-template>
    <xsl:if test="@ac">
      <xsl:element name="sup">
	<xsl:element name="sup">
	  <xsl:text>.</xsl:text>
	</xsl:element>
      </xsl:element>
    </xsl:if>
    <xsl:call-template name="print.params"/>
    <xsl:if test="@cl">
      <xsl:element name="sup">
	<xsl:element name="sup">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	  <xsl:text>plus;</xsl:text>
	</xsl:element>
      </xsl:element>
    </xsl:if>
  </xsl:template>  


<!-- ======================================================================
              Generalization
     ====================================================================== -->
  <!-- ftvs -->
  <xsl:template match="ftvs" mode="formula">
    <xsl:call-template name="print.mathmode">
      <xsl:with-param name="print.mathmode.text">ftv</xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.params"/>
  </xsl:template>  
  
  <!-- V_ -->
  <xsl:template match="V_" mode="formula">
    <xsl:text>V</xsl:text>
    <xsl:element name="sup">    
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>minus;</xsl:text>
    </xsl:element>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="print.children"/>
    <xsl:text>)</xsl:text>    
  </xsl:template>  
  
  <!-- V__ -->
  <xsl:template match="V__" mode="formula">
    <xsl:text>V</xsl:text>
    <xsl:element name="sup">    
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>minus;</xsl:text>
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>minus;</xsl:text>
    </xsl:element>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="print.children"/>
    <xsl:call-template name="print.op.comma"/>
    <xsl:value-of select="@mode"/>    
    <xsl:text>)</xsl:text>
  </xsl:template>

  <!-- GEN -->
  <xsl:template match="GEN" mode="formula">
    <xsl:call-template name="print.infix">
      <xsl:with-param name="print.infix.op">ldot;</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

 <!--  ====================================================================
              Convenience Definitions
        ==================================================================== -->

  <!-- types ...* -->
  <xsl:template match="types" mode="formula">
    <!-- <xsl:text>(</xsl:text> -->    
    <xsl:call-template name="print.type"/>
    <!-- <xsl:text>)</xsl:text> -->    
    <xsl:call-template name="print.plural"/>
  </xsl:template>

  <!-- tvars -->
  <xsl:template match="tvars" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:choose>
      <xsl:when test = "@name">
	<xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>alpha</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
    <xsl:call-template name="print.plural"/>
  </xsl:template>
  
  <!-- Spair -->
  <xsl:template match="Spair" mode="formula">
    <xsl:call-template name="print.comp.actual">
      <xsl:with-param name="print.comp.actual.symbol">otimes;</xsl:with-param> 
      <xsl:with-param name="print.comp.actual.name">pair</xsl:with-param> 
      <xsl:with-param name="print.comp.actual.kind">val</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>
  
  <!-- Ulist -->
  <xsl:template match="Ulist" mode="formula">
    <xsl:call-template name="print.comp.actual">
      <xsl:with-param name="print.comp.actual.symbol">oplus;</xsl:with-param> 
      <xsl:with-param name="print.comp.actual.name">list</xsl:with-param> 
      <xsl:with-param name="print.comp.actual.kind">ref</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- optional -->
  <xsl:template match="optional" mode="formula">
    <xsl:call-template name="print.comp.actual">
      <xsl:with-param name="print.comp.actual.symbol">oplus;</xsl:with-param> 
      <xsl:with-param name="print.comp.actual.name">optional</xsl:with-param> 
      <xsl:with-param name="print.comp.actual.kind">val</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>

  <!-- Madj: Adjust maybe-ness -->
  <xsl:template match="Madj" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>weierp;</xsl:text>
    <xsl:call-template name="print.params"/>
  </xsl:template>  
  
  <!-- UnifiedType -->
  <xsl:template match="UnifiedType" mode="formula">
    <xsl:call-template name="print.procedure">
      <xsl:with-param name="print.procedure.name">UnifiedType</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- TypeOfCopy -->
  <xsl:template match="TypeOfCopy" mode="formula">
    <xsl:call-template name="print.procedure">
      <xsl:with-param name="print.procedure.name">TypeOfCopy</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- typeWithoutHint -->
  <xsl:template match="typeWithoutHint" mode="formula">
    <xsl:call-template name="print.procedure">
      <xsl:with-param name="print.procedure.name">Core</xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  

  <!-- corUp -->
  <xsl:template match="corUp" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:call-template name="print.op.corUp"/>
    <xsl:apply-templates select="*[2]" mode="formula"/>	
  </xsl:template>

  <!-- corDown -->
  <xsl:template match="corDown" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:call-template name="print.op.corUp"/>
    <xsl:apply-templates select="*[2]" mode="formula"/>	
  </xsl:template>

 <!-- ======================================================================
       ======================================================================
              Grammar Description 
       ======================================================================
     ====================================================================== -->
  <!-- Language -->
  <xsl:template match="language" mode="formula">
    <xsl:choose>
      <xsl:when test="@name">
	<xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:when test="@symb">	
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:value-of select="@symb"/>
	<xsl:text>;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="print.mathbb">
	  <xsl:with-param name="print.mathbb.letter">B</xsl:with-param> 
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- grammar -->
  <xsl:template match="grammar" mode="formula">
    <xsl:choose>
      <xsl:when test = "@notitle">
	<!-- nothing -->
      </xsl:when>
      <xsl:otherwise>
	<!-- default -->
	<xsl:element name="p">      
	  <xsl:element name="leadin">
	    <xsl:text>Syntax</xsl:text>
	  </xsl:element>
	</xsl:element>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:element name="table">
      <xsl:attribute name="valign">top</xsl:attribute>
      <xsl:if test="@placement='center'">
	<xsl:attribute name="latex.center">yes</xsl:attribute>
      </xsl:if>
      <xsl:if test="@break='yes'">
	<xsl:attribute name="latex.long">yes</xsl:attribute>
      </xsl:if>
      <xsl:element name="tbody">
	<xsl:apply-templates mode="formula"/>	
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
  <!-- bnf -->
  <xsl:template match="bnf" mode="formula">    
    <xsl:element name="tr">
      <xsl:attribute name="valign">top</xsl:attribute>
      <xsl:element name="td">
	<xsl:element name="p">
	  <xsl:value-of select="@desc"/> 
	</xsl:element>
      </xsl:element>
      <xsl:element name="td">
	<xsl:element name="p">
	  <xsl:apply-templates select="*[1]" mode="formula"/>	
	  <xsl:text> ::= </xsl:text>
	</xsl:element>
      </xsl:element>
      <xsl:element name="td">
	<xsl:element name="p">
	  <xsl:apply-templates select="*[2]" mode="formula"/>	
	</xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <!-- bnf2 -->
  <xsl:template match="bnf2" mode="formula">    
    <xsl:element name="tr">
      <xsl:for-each select="*">
	<xsl:element name="td">
	  <xsl:attribute name="align">left</xsl:attribute>	  
	  <xsl:if test="../@colspan">
	    <xsl:attribute name="colspan">
	      <xsl:value-of select="../@colspan"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:element name="table">
	    <xsl:attribute name="valign">top</xsl:attribute>
	    <xsl:element name="tbody">
	      <xsl:apply-templates select="." mode="formula"/>		
	    </xsl:element>
	  </xsl:element>
	</xsl:element>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>

  <!-- bnfc -->
  <xsl:template match="bnfc" mode="formula">
    <xsl:element name="tr">
      <xsl:attribute name="valign">top</xsl:attribute>
      <xsl:element name="td">
	<xsl:element name="p">
	  <xsl:call-template name="print.space"/>
	  <xsl:call-template name="print.space"/>
	  <xsl:if test="@desc">
	    <xsl:element name="em">
	      <xsl:value-of select="@desc"/> 
	    </xsl:element>
	  </xsl:if>
	</xsl:element>
      </xsl:element>
      <xsl:element name="td">
	<xsl:element name="p">
	  <xsl:call-template name="print.space"/>
	</xsl:element>
      </xsl:element>      
      <xsl:element name="td">
	<xsl:element name="p">
	  <xsl:call-template name="print.op.alt"/>
	  <xsl:apply-templates mode="formula"/>	
	</xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>

 <!-- ======================================================================
      ======================================================================
                                Expressions
      ======================================================================
      ====================================================================== -->

  <!-- aVal -->
  <xsl:template match="aVal" mode="formula">
    <xsl:element name="em">
      <xsl:text>v</xsl:text>
      <xsl:call-template name="print.index.dash"/>
    </xsl:element>
  </xsl:template>  

  <!-- aVal -->
  <xsl:template match="lVal" mode="formula">
    <xsl:element name="em">
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>pound;</xsl:text>    
      <xsl:choose>
	<xsl:when test="@stack">
	  <xsl:element name="sup">
	    <xsl:text>l</xsl:text>    
	  </xsl:element>
	</xsl:when>
	<xsl:when test="@heap">
	  <xsl:element name="sup">
	    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	    <xsl:text>ell;</xsl:text>
	  </xsl:element>
	</xsl:when>
      </xsl:choose>
      <xsl:call-template name="print.index.dash"/>
    </xsl:element>
  </xsl:template>  
  
  <!-- aExpr -->
  <xsl:template match="aExpr" mode="formula">
    <xsl:element name="em">
      <xsl:choose>
	<xsl:when test="@hat='yes'">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	  <xsl:text>euml;</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>e</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="print.index.dash"/>
    </xsl:element>
  </xsl:template>

  <!-- vExp -->
  <xsl:template match="vExp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>upsi;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  

  <!-- Unit -->
  <xsl:template match="Unit" mode="formula">
    <!-- <xsl:element name="progident"> -->
      <xsl:text>()</xsl:text>
  <!-- </xsl:element> -->
  </xsl:template>  

  <!-- true -->
  <xsl:template match="true" mode="formula">
    <xsl:element name="em">
      <xsl:text>true</xsl:text>
    </xsl:element>
  </xsl:template>  

  <!-- false -->
  <xsl:template match="false" mode="formula">
    <xsl:element name="em">
      <xsl:text>false</xsl:text>
    </xsl:element>
  </xsl:template>  

  <!-- id -->
  <xsl:template match="id" mode="formula">
    <xsl:element name="em">
      <xsl:choose>
	<xsl:when test = "@name">
	  <xsl:value-of select="@name"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>x</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:element>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  

  <!-- hLoc -->
  <xsl:template match="hLoc" mode="formula">
    <xsl:element name="em">
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:text>ell;</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  

  <!-- sLoc -->
  <xsl:template match="sLoc" mode="formula">
    <xsl:element name="em">
      <xsl:text>l</xsl:text>    
    </xsl:element>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  

  <!-- loc -->
  <xsl:template match="loc" mode="formula">
    <xsl:call-template name="print.mathcal">
      <xsl:with-param name="print.mathcal.letter">L</xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>  


  <!-- lambda -->
  <xsl:template match="lambda" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>lambda;</xsl:text>    
    <xsl:for-each select="*">
      <xsl:if test = "position() = last()">
	<xsl:text>.</xsl:text>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>  

  <!-- Pair -->
  <xsl:template match="Pair" mode="formula">
    <xsl:element name="progident">
      <xsl:text>(</xsl:text>
    </xsl:element>
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:element name="progident">
      <xsl:text>, </xsl:text>
    </xsl:element>
    <xsl:apply-templates select="*[2]" mode="formula"/>
    <xsl:element name="progident">
      <xsl:text>)</xsl:text>
    </xsl:element>
  </xsl:template>

  <!-- List -->
  <xsl:template match="List" mode="formula">
    <xsl:element name="progident">
      <xsl:text>[</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.children">
      <xsl:with-param name="print.children.sep">;</xsl:with-param>
    </xsl:call-template>    
    <xsl:element name="progident">
      <xsl:text>]</xsl:text>
    </xsl:element>
  </xsl:template>
  
  <!-- fst -->
  <xsl:template match="fst" mode="formula">
    <xsl:apply-templates mode="formula"/>
    <xsl:element name="progident">
      <xsl:text>.1</xsl:text>
    </xsl:element>
  </xsl:template>

  <!-- snd -->
  <xsl:template match="snd" mode="formula">
    <xsl:apply-templates mode="formula"/>
    <xsl:element name="progident">
      <xsl:text>.2</xsl:text>
    </xsl:element>
  </xsl:template>

  <!-- select -->
  <xsl:template match="select" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:if test = "@opt">	      
      <xsl:text>[</xsl:text>	  
    </xsl:if>    
    <xsl:element name="progident">
      <xsl:text>.</xsl:text>
    </xsl:element>
    <xsl:apply-templates select="*[2]" mode="formula"/>
    <xsl:if test = "@opt">	      
      <xsl:text>]</xsl:text>	  
    </xsl:if>    
  </xsl:template>

  <!-- ith -->
  <xsl:template match="ith" mode="formula">
    <xsl:apply-templates mode="formula"/>
    <xsl:if test="@etc = 'yes'">
      <xsl:text>...</xsl:text>
    </xsl:if>
    <xsl:element name="progident">
      <xsl:text>.</xsl:text>
    </xsl:element>
    <xsl:element name="em">
      <xsl:text>i</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- path -->
  <xsl:template match="path" mode="formula">
    <xsl:call-template name="print.mathmode">
      <xsl:with-param name="print.mathmode.text">p</xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>
  
  <!-- match -->
  <xsl:template match="match" mode="formula">
    <xsl:element name="progident">
      <xsl:text>match</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.space"/>    
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.space"/>    
    <xsl:element name="progident">
      <xsl:text>with</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.space"/>    
    <xsl:apply-templates select="*[2]" mode="formula"/>
    <xsl:call-template name="print.space"/>    
    <xsl:element name="progident">
      <xsl:text>in</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.space"/>    
    <xsl:if test="@br1">
      <xsl:element name="br"/>
      <xsl:call-template name="print.spaces">
	<xsl:with-param name="print.spaces.n">
	  <xsl:value-of select="@br1"/>
	</xsl:with-param> 
      </xsl:call-template>
    </xsl:if>    
    <xsl:apply-templates select="*[3]" mode="formula"/>
    <xsl:call-template name="print.space"/>        
    <xsl:if test = "*[4]">	      
      <xsl:if test = "@optionalElse">	      
	<xsl:text>[</xsl:text>	  
      </xsl:if>    
      <xsl:element name="progident">
	<xsl:text>else</xsl:text>
      </xsl:element>
      <xsl:call-template name="print.space"/>    
      <xsl:if test="@br2">
	<xsl:element name="br"/>
	<xsl:call-template name="print.spaces">
	  <xsl:with-param name="print.spaces.n">
	    <xsl:value-of select="@br2"/>
	  </xsl:with-param> 
	</xsl:call-template>
      </xsl:if>    
      <xsl:apply-templates select="*[4]" mode="formula"/>
      <xsl:if test = "@optionalElse">	      
	<xsl:text>]</xsl:text>	  
      </xsl:if>    
    </xsl:if>
  </xsl:template>

  <!-- pattern -->
  <xsl:template match="pattern" mode="formula">
    <xsl:element name="progident">
      <xsl:text>p</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- Cons -->
  <xsl:template match="Cons" mode="formula">
    <xsl:call-template name="print.children">
      <xsl:with-param name="print.children.septt">::</xsl:with-param>
      <xsl:with-param name="print.children.nosp">yes</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  
  <!-- apply -->
  <xsl:template match="apply" mode="formula">
    <xsl:for-each select="*">
      <xsl:if test = "position() &gt; 1">
	<xsl:call-template name="print.space"/>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>  
  
  <!-- tqExpr -->
  <xsl:template match="tqExpr" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:choose>
      <xsl:when test = "@optional">	      
	<xsl:text>[</xsl:text>	  
	<xsl:call-template name="print.op.qual.bare"/>
	<xsl:apply-templates select="*[2]" mode="formula"/>
	<xsl:text>]</xsl:text>    
      </xsl:when>
      <xsl:when test = "@nosp">	      
	<xsl:call-template name="print.op.qual"/>
	<xsl:apply-templates select="*[2]" mode="formula"/> 
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="print.op.qual"/>
	<xsl:apply-templates select="*[2]" mode="formula"/> 
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>  
  
  <!-- dup -->
  <xsl:template match="dup" mode="formula">
    <xsl:element name="progident">
      <xsl:text>dup</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.params"/>
  </xsl:template>  

  <!-- deref -->
  <xsl:template match="deref" mode="formula">
    <xsl:apply-templates mode="formula"/>
    <xsl:element name="progident">
      <xsl:text>^</xsl:text>
    </xsl:element>
  </xsl:template>  

  <!-- let -->
  <xsl:template match="let" mode="formula">
    <xsl:call-template name="print.kwd">
      <xsl:with-param name="print.kwd.kwd">let</xsl:with-param> 
    </xsl:call-template>
    <xsl:if test="@kind='p'">
      <xsl:element name="sup">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>forall;</xsl:text>
      </xsl:element>
    </xsl:if>
    <xsl:if test="@kind='m'">
      <xsl:element name="sup">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>psi;</xsl:text>
      </xsl:element>
    </xsl:if>
    <xsl:if test="@kind='k'">
      <xsl:element name="sup">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>kappa;</xsl:text>
      </xsl:element>
    </xsl:if>
    <xsl:if test="@kind='x'">
      <xsl:element name="sup">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>kappav;</xsl:text>
      </xsl:element>
    </xsl:if>
    <xsl:if test="*[1]">
      <xsl:call-template name="print.space"/>        
      <xsl:apply-templates select="*[1]" mode="formula"/>
      <xsl:call-template name="print.op.eq"/>
      <xsl:if test="@br1">
	<xsl:element name="br"/>
	<xsl:call-template name="print.spaces">
	  <xsl:with-param name="print.spaces.n">
	    <xsl:value-of select="@br1"/>
	  </xsl:with-param> 
	</xsl:call-template>
      </xsl:if>    
      <xsl:apply-templates select="*[2]" mode="formula"/>
      <xsl:if test="*[3]">
	<xsl:call-template name="print.skwds">
	  <xsl:with-param name="print.skwds.kwd">in</xsl:with-param> 
	</xsl:call-template>
	<xsl:if test="@br2">
	  <xsl:element name="br"/>
	  <xsl:call-template name="print.spaces">
	    <xsl:with-param name="print.spaces.n">
	      <xsl:value-of select="@br2"/>
	    </xsl:with-param> 
	  </xsl:call-template>
	</xsl:if>    
	<xsl:apply-templates select="*[3]" mode="formula"/>
      </xsl:if>
    </xsl:if>
  </xsl:template>  

  <!-- letrec -->
  <xsl:template match="letrec" mode="formula">
    <xsl:call-template name="print.kwds">
      <xsl:with-param name="print.kwds.kwd">letrec</xsl:with-param> 
    </xsl:call-template>
    <xsl:for-each select="*">
      <xsl:if test = "position() = last() -1">	
	<xsl:call-template name="print.op.eq"/>
      </xsl:if>
      <xsl:if test = "position() = last()">
	<xsl:call-template name="print.skwds">
	  <xsl:with-param name="print.skwds.kwd">in</xsl:with-param> 
	</xsl:call-template>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>  

  <!-- let Kinds -->
  <xsl:template match="lKind" mode="formula">
    <xsl:choose>
      <xsl:when test="@k='poly'">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>forall;</xsl:text>
      </xsl:when>
      <xsl:when test="@k='mono'">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>psi;</xsl:text>
      </xsl:when>
      <xsl:when test="@k='var'">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>kappa;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
 	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>kappav;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- aProgram -->
  <xsl:template match="aProgram" mode="formula">
    <xsl:text>P</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- define -->
  <xsl:template match="define" mode="formula">
    <xsl:call-template name="print.kwds">
      <xsl:with-param name="print.kwds.kwd">define</xsl:with-param> 
    </xsl:call-template>
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.op.eq"/>
    <xsl:apply-templates select="*[2]" mode="formula"/>
    <xsl:call-template name="print.skwds">
      <xsl:with-param name="print.skwds.kwd">in</xsl:with-param> 
    </xsl:call-template>
    <xsl:apply-templates select="*[3]" mode="formula"/>
  </xsl:template>  

  <!-- proclaim -->
  <xsl:template match="proclaim" mode="formula">
    <xsl:call-template name="print.kwds">
      <xsl:with-param name="print.kwds.kwd">proclaim</xsl:with-param> 
    </xsl:call-template>
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.skwds">
      <xsl:with-param name="print.skwds.kwd">in</xsl:with-param> 
    </xsl:call-template>    
    <xsl:apply-templates select="*[2]" mode="formula"/>
  </xsl:template>  
  
  <!-- assign -->
  <xsl:template match="assign" mode="formula">
    <xsl:for-each select="*">
      <xsl:if test = "position() = last()">
	<xsl:call-template name="print.op.assign"/>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>  

  <!-- if -->
  <xsl:template match="if" mode="formula">
    <xsl:call-template name="print.kwds">
      <xsl:with-param name="print.kwds.kwd">if</xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.space"/>    
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:if test="@br1">
      <xsl:element name="br"/>
      <xsl:call-template name="print.spaces">
	<xsl:with-param name="print.spaces.n">
	  <xsl:value-of select="@br1"/>
	</xsl:with-param> 
      </xsl:call-template>
    </xsl:if>    
    <xsl:call-template name="print.skwds">
      <xsl:with-param name="print.skwds.kwd">then</xsl:with-param> 
    </xsl:call-template>
    <xsl:apply-templates select="*[2]" mode="formula"/>
    <xsl:if test="@br2">
      <xsl:element name="br"/>
      <xsl:call-template name="print.spaces">
	<xsl:with-param name="print.spaces.n">
	  <xsl:value-of select="@br2"/>
	</xsl:with-param> 
      </xsl:call-template>
    </xsl:if>    
    <xsl:call-template name="print.skwds">
      <xsl:with-param name="print.skwds.kwd">else</xsl:with-param> 
    </xsl:call-template>
    <xsl:apply-templates select="*[3]" mode="formula"/>
  </xsl:template>  

  <!-- opt: Optional parts of syntax (of anything actually) -->
  <xsl:template match="opt" mode="formula">
    <xsl:text>[</xsl:text>	  
    <xsl:apply-templates mode="formula"/>	
    <xsl:text>]</xsl:text>	  
  </xsl:template>    
  
  <!-- ======================================================================
  ======================================================================
                                Semantics
  ======================================================================
      ====================================================================== -->


<!-- ======================================================================
                              Contexts
     ====================================================================== --> 

  <!-- extend -->
  <xsl:template match="extend" mode="formula">
    <xsl:call-template name="print.extn"/>
  </xsl:template>  

  <!-- Stack -->
  <xsl:template match="stack" mode="formula">
    <xsl:text>K</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- Heap -->
  <xsl:template match="heap" mode="formula">
    <xsl:text>H</xsl:text>    
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>
  
  <!-- gamma -->
  <xsl:template match="gamma" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Gamma;</xsl:text>    
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- store -->
  <xsl:template match="store" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Sigma;</xsl:text>    
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

  <!-- ctEnv -->
  <xsl:template match="ctEnv" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Pi;</xsl:text>    
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>

<!-- ======================================================================
                              Operational Semantics
     ====================================================================== -->

  <!-- opState -->
  <xsl:template match="opState" mode="formula">
    <xsl:for-each select="*">
      <xsl:if test = "position() &gt; 1">
	<xsl:call-template name="print.op.semis"/>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>  
  
  <!-- evalOp -->
  <xsl:template match="evalOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rArr;</xsl:text>    
    <xsl:if test="@many">
      <xsl:text>*</xsl:text>    
    </xsl:if>
  </xsl:template>    
  <!-- levalOp -->
  <xsl:template match="levalOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rAarr;</xsl:text>    
  </xsl:template>  
  <!-- revalOp -->
  <xsl:template match="revalOp" mode="formula">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>xrArr;</xsl:text>    
    <xsl:element name="sup">
      <xsl:text>r</xsl:text>
    </xsl:element>
  </xsl:template>  

  <!-- eval -->
  <xsl:template match="eval" mode="formula">
    <xsl:for-each select="*">
      <xsl:if test = "position() &gt; 1">
	<xsl:choose>
	  <xsl:when test="../@many">	  
	    <xsl:call-template name="print.op.eval">
	      <xsl:with-param name="print.op.eval.many">*</xsl:with-param> 
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="print.op.eval"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>    
  <!-- leval -->
  <xsl:template match="leval" mode="formula">
    <xsl:for-each select="*">
      <xsl:if test = "position() &gt; 1">
	<xsl:call-template name="print.op.leval"/>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>  
  <!-- reval -->
  <xsl:template match="reval" mode="formula">
    <xsl:for-each select="*">
      <xsl:if test = "position() &gt; 1">
	<xsl:call-template name="print.op.reval"/>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>  

  <!-- opPre -->
  <xsl:template match="opPre" mode="formula">
    <xsl:call-template name="print.premices"/>
  </xsl:template>

  <!-- opConc -->
  <xsl:template match="opConc" mode="formula">
    <xsl:apply-templates mode="formula"/>
  </xsl:template>
  
  <!-- opRule -->
  <xsl:template match="opRule" mode="formula">
    <xsl:call-template name="print.rule"/>
  </xsl:template>

  <!-- opRule -->
  <xsl:template match="opHorzRules" mode="formula">
    <xsl:element name="table">
      <xsl:element name="tbody">
	<xsl:element name="tr">
	  <xsl:attribute name="valign">top</xsl:attribute>
	  <xsl:attribute name="lineafter">yes</xsl:attribute>
	  <xsl:element name="td">
	    <xsl:element name="p">		
	      <xsl:text>Rule</xsl:text>
	    </xsl:element>
	  </xsl:element>
	  <xsl:element name="td">
	    <xsl:element name="p">		
	      <xsl:text>Pre-conditions</xsl:text>
	    </xsl:element>
	  </xsl:element>
	  <xsl:element name="td">
	    <xsl:element name="p">		
	      <xsl:text>Evaluation Step</xsl:text>
	    </xsl:element>
	  </xsl:element>
	</xsl:element>	
      </xsl:element>
      <xsl:for-each select="*">
	<xsl:element name="tr">
	  <xsl:attribute name="valign">top</xsl:attribute>
	  <!-- <xsl:attribute name="lineafter">yes</xsl:attribute> -->
	  <xsl:element name="td">
	    <xsl:element name="p">				    
	      <xsl:apply-templates select="@name" mode="formula"/>	
	    </xsl:element>
	  </xsl:element>
	  <xsl:element name="td">
	    <xsl:element name="p">				    
	      <xsl:apply-templates select="*[1]" mode="formula"/>	
	    </xsl:element>
	  </xsl:element>
	  <xsl:element name="td">
	    <xsl:element name="p">				    
	      <xsl:apply-templates select="*[2]" mode="formula"/>	
	    </xsl:element>
	  </xsl:element>
	</xsl:element>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>

  <!-- statement -->
  <xsl:template match="statement" mode="formula">
    <xsl:element name="tr">
      <xsl:element name="td">
	<xsl:apply-templates mode="formula"/>      
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <!-- equation -->
  <xsl:template match="equation" mode="formula">
    <xsl:element name="tr">
      <xsl:attribute name="valign">top</xsl:attribute>
      
      <!-- LHS -->
      <xsl:element name="td">
	<xsl:attribute name="align">right</xsl:attribute>
	<xsl:apply-templates select="*[1]" mode="formula"/>
      </xsl:element>
      
      <!-- the = sign -->
      <xsl:element name="td">
	<xsl:text>=</xsl:text>
      </xsl:element>
      
      <!-- RHS -->      
      <xsl:element name="td">
	<xsl:attribute name="align">left</xsl:attribute>	  
	<xsl:apply-templates select="*[2]" mode="formula"/>	
      </xsl:element>
      <xsl:if test="@sep">
	<xsl:element name="br">
	  <xsl:attribute name="latex.ptsz">
	    <xsl:value-of select="@sep"/>
	  </xsl:attribute>
	</xsl:element>
      </xsl:if>          
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="eqn-cnt" mode="formula">
    <xsl:element name="tr">
      <xsl:attribute name="valign">top</xsl:attribute>
      
      <!-- STUB for LHS -->
      <xsl:element name="td">
      </xsl:element>
      
      <!-- STUB for the = sign -->
      <xsl:element name="td">
      </xsl:element>
      
      <!-- RHS -->      
      <xsl:element name="td">
	<xsl:attribute name="align">left</xsl:attribute>	  
	<xsl:apply-templates select="*[1]" mode="formula"/>	
      </xsl:element>
      <xsl:if test="@sep">
	<xsl:element name="br">
	  <xsl:attribute name="latex.ptsz">
	    <xsl:value-of select="@sep"/>
	  </xsl:attribute>
	</xsl:element>
      </xsl:if>          
    </xsl:element>
  </xsl:template>

  <!-- lhs -->
  <xsl:template match="lhs" mode="formula">
    <xsl:call-template name="print.eqnside"/>    
  </xsl:template>

  <!-- rhs -->
  <xsl:template match="rhs" mode="formula">
    <xsl:call-template name="print.eqnside"/>
  </xsl:template>

  <!-- VEqns -->
  <xsl:template match="VEqns" mode="formula">
    <xsl:element name="table">
      <xsl:attribute name="valign">top</xsl:attribute>      
      <xsl:attribute name="latex.center">yes</xsl:attribute>
      <xsl:element name="tbody">
	<xsl:apply-templates mode="formula"/>	
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
<!-- ======================================================================
                        Modelling / Satisfaction 
     ====================================================================== -->

  <!-- models -->
  <xsl:template match="models" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.models">
      <xsl:with-param name="print.models.name">
	<xsl:value-of select="@name"/>
      </xsl:with-param> 
    </xsl:call-template>    	
    <xsl:apply-templates select="*[2]" mode="formula"/>
  </xsl:template>

  <!-- Cst -->
  <xsl:template match="Cst" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.cst">
      <xsl:with-param name="print.cst.name">
	<xsl:value-of select="@name"/>
      </xsl:with-param> 
    </xsl:call-template>    	
    <xsl:apply-templates select="*[2]" mode="formula"/>
  </xsl:template>

  <!-- CST -->
  <xsl:template match="CST" mode="formula">
    <xsl:call-template name="print.cst">
      <xsl:with-param name="print.cst.name">
	<xsl:value-of select="@name"/>
      </xsl:with-param> 
    </xsl:call-template>
    <xsl:choose>
      <xsl:when test="@sp">
	<xsl:call-template name="print.spset"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="print.set"/>
      </xsl:otherwise>
    </xsl:choose>	
  </xsl:template>

  <!-- Sat -->
  <xsl:template match="Sat" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.sat">
      <xsl:with-param name="print.sat.name">
	<xsl:value-of select="@name"/>
      </xsl:with-param> 
    </xsl:call-template>    	
    <xsl:apply-templates select="*[2]" mode="formula"/>
  </xsl:template>
  
  
<!-- ======================================================================
                       Type Judgements 
     ====================================================================== -->

  <!-- assume -->
  <xsl:template match="assume" mode="formula">
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() &lt; last()">
	<xsl:call-template name="print.op.semis"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <!-- conclude -->
  <xsl:template match="conclude" mode="formula">
    <xsl:choose>
      <xsl:when test= "../@name">
	<xsl:call-template name="print.derives">
	  <xsl:with-param name="print.derives.name">
	    <xsl:value-of select="../@name"/>
	  </xsl:with-param> 
	</xsl:call-template>
      </xsl:when>
      <xsl:when test= "parent::TIjudge">
	<xsl:call-template name="print.infer"/>
      </xsl:when>
      <xsl:when test= "parent::TEjudge">
	<xsl:call-template name="print.eqinfer"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="print.derives"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates mode="formula"/>
  </xsl:template>

  <!-- constrain -->
  <xsl:template match="constrain" mode="formula">
    <xsl:call-template name="print.op.unfct"/>	
    <xsl:apply-templates mode="formula"/>
  </xsl:template>
  
  <!-- precond -->
  <xsl:template match="precond" mode="formula">
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() &lt; last()">
	<xsl:call-template name="print.op.semis"/>
      </xsl:if>
    </xsl:for-each>
    <xsl:call-template name="print.op.par"/>
  </xsl:template>

  <!-- propagate -->
  <xsl:template match="propagate" mode="formula">
    <xsl:call-template name="print.op.par"/>	
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() &lt; last()">
	<xsl:call-template name="print.op.semis"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <!-- judgeOp -->
  <xsl:template match="judgeOp" mode="formula">
    <xsl:call-template name="print.derives">
      <xsl:with-param name="print.derives.name">
	<xsl:value-of select="@name"/>
      </xsl:with-param> 
      <xsl:with-param name="print.derives.sym">
	<xsl:value-of select="@sym"/>
      </xsl:with-param> 
    </xsl:call-template>
  </xsl:template>  
  
  <!-- judge -->
  <xsl:template match="judge" mode="formula">
    <xsl:apply-templates mode="formula"/>
  </xsl:template>  

  <!-- Sjudge -->
  <!--  assumption |- inlined conclusion -->
  <xsl:template match="Sjudge" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.derives">	
      <xsl:with-param name="print.derives.name">	
	<xsl:value-of select="@name"/>
      </xsl:with-param> 
    </xsl:call-template>    	
    <xsl:apply-templates select="*[2]" mode="formula"/>
  </xsl:template>  

  <!-- Djudge -->
  <!-- submap || gamma; store |-diamond expr : type -->
  <xsl:template match="Djudge" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.op.par"/>
    <xsl:apply-templates select="*[2]" mode="formula"/>
    <xsl:call-template name="print.op.semis"/>	
    <xsl:apply-templates select="*[3]" mode="formula"/>
    <xsl:call-template name="print.derives">
      <xsl:with-param name="print.derives.sym">diam</xsl:with-param> 
    </xsl:call-template>
    <xsl:apply-templates select="*[4]" mode="formula"/>
    <xsl:call-template name="print.op.qual"/>	
    <xsl:apply-templates select="*[5]" mode="formula"/>
  </xsl:template>    

  <!-- Cjudge -->
  <!-- submap || gamma; store |-diamond expr : type -->
  <xsl:template match="Cjudge" mode="formula">
    <xsl:apply-templates select="*[1]" mode="formula"/>
    <xsl:call-template name="print.op.semis"/>
    <xsl:apply-templates select="*[2]" mode="formula"/>
    <xsl:call-template name="print.op.semis"/>	
    <xsl:apply-templates select="*[3]" mode="formula"/>
    <xsl:call-template name="print.derives">
      <!--       <xsl:with-param name="print.derives.sym">compfn</xsl:with-param>  -->
      <xsl:with-param name="print.derives.name">*</xsl:with-param> 
    </xsl:call-template>
    <xsl:apply-templates select="*[4]" mode="formula"/>
    <xsl:call-template name="print.op.qual"/>	
    <xsl:apply-templates select="*[5]" mode="formula"/>
  </xsl:template>    
  
  <!-- TDjudge -->
  <!-- gamma; store |-D expr : type , all inlined-->
  <xsl:template match="TDjudge" mode="formula">
    <xsl:if test = "@cst='yes'">	
      <xsl:call-template name="print.mathcal">
	<xsl:with-param name="print.mathcal.letter">C</xsl:with-param> 
      </xsl:call-template>
      <xsl:call-template name="print.op.semis"/>	
    </xsl:if>      
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Gamma;</xsl:text>
    <xsl:call-template name="print.op.semis"/>	
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Sigma;</xsl:text>
    <xsl:call-template name="print.derives">
      <xsl:with-param name="print.derives.name">
	<xsl:value-of select="@name"/>
      </xsl:with-param>
    </xsl:call-template>	
    <xsl:for-each select="*">
      <xsl:if test = "position() = 2">	
	<xsl:choose>
 	  <xsl:when test = "../@sub">
	    <xsl:call-template name="print.op.subqual"/>	
	  </xsl:when>
 	  <xsl:when test = "../@Msub">
	    <xsl:call-template name="print.op.Mqual"/>	
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="print.op.qual"/>	
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>  
  
  <!-- TIjudge -->
  <xsl:template match="TIjudge" mode="formula">
    <xsl:apply-templates mode="formula"/>
  </xsl:template>  

  <!-- TEjudge -->
  <xsl:template match="TEjudge" mode="formula">
    <xsl:apply-templates mode="formula"/>
  </xsl:template>  
  
  <!-- tyPre -->
  <xsl:template match="tyPre" mode="formula">
    <xsl:call-template name="print.premices"/>
  </xsl:template>

  <!-- tyConc -->
  <xsl:template match="tyConc" mode="formula">
    <xsl:apply-templates mode="formula"/>
  </xsl:template>
  
  <!-- tyRule -->
  <xsl:template match="tyRule" mode="formula">
    <xsl:call-template name="print.rule"/>
  </xsl:template>
 
<!-- ======================================================================
              Common to all Logical rules 
     ====================================================================== -->

  <!-- Hrules -->
  <xsl:template match="Hrules" mode="formula">
    <xsl:element name="table">      
      <xsl:if test="@placement='center'">
	<xsl:attribute name="latex.center">yes</xsl:attribute>
      </xsl:if>      
      <xsl:element name="tbody">
	<xsl:element name="tr">
	  <xsl:if test="@initsep">
	    <xsl:element name="td">
	      <xsl:call-template name="print.spaces">
		<xsl:with-param name="print.spaces.n">
		  <xsl:value-of select="@initsep"/>
		</xsl:with-param>
	      </xsl:call-template>
	    </xsl:element>
	  </xsl:if>
	  <xsl:for-each select="*">	    
	    <xsl:element name="td">
	      <xsl:choose>
		<xsl:when test="../@align='left'">
		  <!-- <xsl:attribute name="align">left</xsl:attribute> -->
		</xsl:when>
		<xsl:when test="../@align='right'">
		  <xsl:attribute name="align">right</xsl:attribute>
		</xsl:when>
		<xsl:otherwise>
		  <!-- default is center -->
		  <xsl:attribute name="align">center</xsl:attribute>
		</xsl:otherwise>
	      </xsl:choose>		  
	      <xsl:apply-templates select="." mode="formula"/>
	    </xsl:element>
	    <xsl:if test="../@colsep">
	      <xsl:if test="position() &lt; last()">
		<xsl:element name="td">
		  <xsl:call-template name="print.spaces">
		    <xsl:with-param name="print.spaces.n">
		      <xsl:value-of select="../@colsep"/>
		    </xsl:with-param> 
		  </xsl:call-template>
		</xsl:element>
	      </xsl:if>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:element>
      </xsl:element>
    </xsl:element>
    <xsl:if test="@rowsep">
      <xsl:element name="br">
	<xsl:attribute name="latex.ptsz">
	  <xsl:value-of select="@rowsep"/>
	</xsl:attribute>
      </xsl:element>
    </xsl:if>    
  </xsl:template>
  
  
<!-- ======================================================================
     ======================================================================
                   Theorems and Proofs 
     ======================================================================
     ====================================================================== -->
  
<!-- ======================================================================
                     Definitions 
     ====================================================================== -->

<!-- ======================================================================
                     Citations 
     ====================================================================== -->

  <!-- definition -->
  <xsl:template match="defn" mode="formula">
    <xsl:text>Definition</xsl:text>
    <xsl:call-template name="print.space"/>
    <xsl:call-template name="print.cite"/>
  </xsl:template>  

  <!-- lemma -->
  <xsl:template match="lem" mode="formula">
    <xsl:text>Lemma</xsl:text>
    <xsl:call-template name="print.space"/>
    <xsl:call-template name="print.cite"/>
  </xsl:template>  

  <!-- theorem -->
  <xsl:template match="thm" mode="formula">
    <xsl:text>Theorem</xsl:text>
    <xsl:call-template name="print.space"/>
    <xsl:call-template name="print.cite"/>
  </xsl:template>  

  <!-- Assumption -->
  <xsl:template match="asm" mode="formula">
    <xsl:text>assumption</xsl:text>
    <xsl:call-template name="print.space"/>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="print.cite"/>
    <xsl:text>)</xsl:text>
  </xsl:template>  
  
  <!-- Assumptions -->
  <xsl:template match="asms" mode="formula">
    <xsl:text>assumptions</xsl:text>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="print.space"/>
    <xsl:call-template name="print.cites"/>
    <xsl:text>)</xsl:text>
  </xsl:template>  

  <!-- conclusion -->
  <xsl:template match="conc" mode="formula">
    <xsl:text>conclusion</xsl:text>
    <xsl:call-template name="print.space"/>
    <xsl:call-template name="print.cite">
      <xsl:with-param name="print.cite.paren">yes</xsl:with-param>
    </xsl:call-template>
  </xsl:template>  

  <!-- Case -->
  <xsl:template match="case" mode="formula">
    <xsl:text>case</xsl:text>
    <xsl:call-template name="print.space"/>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="print.cite"/>
    <xsl:text>)</xsl:text>
  </xsl:template>  
  
  <!-- Cases -->
  <xsl:template match="cases" mode="formula">
    <xsl:text>cases</xsl:text>
    <xsl:call-template name="print.space"/>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="print.cites"/>
    <xsl:text>)</xsl:text>
  </xsl:template>  

 <!-- ======================================================================
      ======================================================================
                                Worker Definitions
      ======================================================================
      ====================================================================== -->

   
  <!--  ====================================================================
                     Space handling, Recursion
        ==================================================================== -->

  <!-- Space handling -->
  <xsl:template match="text()" mode="formula">
  </xsl:template>

  <!-- <br/> OK in formula mode -->
  <xsl:template match="br" mode="formula">
    <xsl:element name="br"/>
  </xsl:template>

  <!-- I think this comment is stale -->
  <!-- Handle the defaulting behavior. We don't use this, but we
  *would* use it if we wanted to handle this stuff inline. -->
  <xsl:template match="*">
    <xsl:element name="{name()}">
      <xsl:apply-templates select="*|@*|text()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="@*">
    <xsl:attribute name="{name()}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:value-of select="."/>
  </xsl:template>


  <!--  ====================================================================
                     Worker functions
        ==================================================================== -->

  <!-- Emit a non-breakable space to the output -->
  <xsl:template name="print.space">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>nbsp;</xsl:text>
  </xsl:template>

  <!-- Emit a breakable space to the output -->
  <xsl:template name="print.bspace">
    <xsl:text> </xsl:text>
  </xsl:template>

  <!-- Mathematical modes for characters -->
  <!-- mathmode -->
  <xsl:template name="print.mathmode">
    <xsl:param name="print.mathmode.text"/>
    <xsl:element name="b">
      <xsl:element name="em">
	<xsl:text>?</xsl:text>	
      </xsl:element>
    </xsl:element>
    <xsl:value-of select="$print.mathmode.text"/>
    <xsl:element name="b">
      <xsl:element name="em">
	<xsl:text>?</xsl:text>	
      </xsl:element>
    </xsl:element>    
  </xsl:template>

  <!-- mathbb -->
  <xsl:template name="print.mathbb">
    <xsl:param name="print.mathbb.letter"/>
    <xsl:element name="b">
      <xsl:text>*</xsl:text>	
    </xsl:element>
    <xsl:value-of select="$print.mathbb.letter"/>
    <xsl:element name="b">
      <xsl:text>*</xsl:text>	
    </xsl:element>    
  </xsl:template>
  
  <!-- mathcal -->
  <xsl:template name="print.mathcal">
    <xsl:param name="print.mathcal.letter"/>
    <xsl:element name="em">
      <xsl:text>*</xsl:text>	
    </xsl:element>
    <xsl:value-of select="$print.mathcal.letter"/>
    <xsl:element name="em">
      <xsl:text>*</xsl:text>	
    </xsl:element>    
  </xsl:template>

  <!-- mathfrac -->
  <xsl:template name="print.mathfrac">
    <xsl:param name="print.mathfrac.letter"/>
    <xsl:element name="b">
      <xsl:element name="em">
	<xsl:text>*</xsl:text>	
      </xsl:element>
    </xsl:element>
    <xsl:value-of select="$print.mathfrac.letter"/>
    <xsl:element name="b">
      <xsl:element name="em">
      <xsl:text>*</xsl:text>	
      </xsl:element>
    </xsl:element>    
  </xsl:template>

  <!-- Print any type -->
  <xsl:template name="print.type">
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>tau;</xsl:text>
    <xsl:call-template name="print.index.dash"/>
  </xsl:template>
  

  <!-- Print the * plural notation -->
  <xsl:template name="print.plural">
    <xsl:element name="sup">    
      <xsl:text>*</xsl:text>
    </xsl:element>    
  </xsl:template>

  <!-- the . operator -->
  <xsl:template name="print.op.dot">
    <xsl:text>.</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>  
  <!-- the . operator -->
  <xsl:template name="print.op.sdot">
    <xsl:call-template name="print.space"/>
    <xsl:text>.</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the , operator -->
  <xsl:template name="print.op.comma">
    <xsl:text>,</xsl:text>    
   <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the ; operator -->
  <xsl:template name="print.op.semi">
    <xsl:text>;</xsl:text>    
   <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the sp;sp operator -->
  <xsl:template name="print.op.semis">
    <!--  xsl:call-template name="print.space"/> -->
    <xsl:text>;</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the x operator -->
  <xsl:template name="print.op.times">
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>cross;</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>
  
  <!-- the || operator -->
  <xsl:template name="print.op.par">
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>par;</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the / operator -->
  <xsl:template name="print.op.fs">
    <xsl:call-template name="print.space"/>
    <xsl:text>/</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the = operator -->
  <xsl:template name="print.op.eq">
    <xsl:call-template name="print.space"/>
    <xsl:text>=</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the L-> operator -->
  <xsl:template name="print.op.lnk">
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rarrhk;</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the >-> operator -->
  <xsl:template name="print.op.sub">
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rarrtl;</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>


  <!-- the >> operator -->
  <xsl:template name="print.op.gg">
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Gt;</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>
  
  <!-- the << operator -->
  <xsl:template name="print.op.ll">
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Lt;</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the :>> operator -->
  <xsl:template name="print.op.corUp">
    <xsl:call-template name="print.space"/>
    <xsl:text>:</xsl:text>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Gt;</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>
  
  <!-- the <<: operator -->
  <xsl:template name="print.op.corDown">
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Lt;</xsl:text>    
    <xsl:text>:</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>
  
  <!-- the in operator -->
  <xsl:template name="print.op.in">
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>isin;</xsl:text> 
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the notin operator -->
  <xsl:template name="print.op.notin">
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>notin;</xsl:text> 
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the derives operator -->
  <xsl:template name="print.derives">
    <xsl:param name="print.derives.name"/>
    <xsl:param name="print.derives.sym"/>

    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>vdash;</xsl:text> 
    <xsl:choose>      
      <xsl:when test="$print.derives.name">
	<xsl:element name="sub">
	  <xsl:value-of select="$print.derives.name"/>
	</xsl:element>
      </xsl:when>
      <xsl:when test="$print.derives.sym">
	<xsl:element name="sub">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	  <xsl:value-of select="$print.derives.sym"/>
	  <xsl:text>;</xsl:text>
	</xsl:element>
      </xsl:when>
    </xsl:choose>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the models |= operator -->
  <xsl:template name="print.models">
    <xsl:param name="print.models.name"/>
    
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>models;</xsl:text> 
    <xsl:if test="$print.models.name">
      <xsl:element name="sub">
	<xsl:value-of select="$print.models.name"/>
      </xsl:element>
    </xsl:if>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the consistency |||- operator -->
  <xsl:template name="print.cst">
    <xsl:param name="print.cst.name"/>
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Vvdash;</xsl:text> 
    <xsl:if test="$print.cst.name">
      <xsl:element name="sub">
	<xsl:value-of select="$print.cst.name"/>
      </xsl:element>
    </xsl:if>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the Satisfiability ||- operator -->
  <xsl:template name="print.sat">
    <xsl:param name="print.sat.name"/>
    
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>Vdash;</xsl:text> 
    <xsl:if test="$print.sat.name">
      <xsl:element name="sub">
	<xsl:value-of select="$print.sat.name"/>
      </xsl:element>
    </xsl:if>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- |-inf infer operator -->
  <xsl:template name="print.infer">
    <xsl:call-template name="print.derives">
      <xsl:with-param name="print.derives.name">i</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- |-eq eqinfer operator -->
  <xsl:template name="print.eqinfer">
    <xsl:call-template name="print.derives">
      <xsl:with-param name="print.derives.name">eq</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- the | (unfct) operator -->
  <xsl:template name="print.op.unfct">
    <xsl:call-template name="print.space"/>
    <xsl:element name="progident">
      <xsl:text>|</xsl:text>
    </xsl:element>	
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the | (alternatives) operator -->
  <xsl:template name="print.op.alt">
    <xsl:call-template name="print.space"/>
    <xsl:element name="progident">
      <xsl:text>|</xsl:text>
    </xsl:element>	
    <xsl:call-template name="print.space"/>
  </xsl:template>
  
  <!-- the ~> (result) operator -->
  <xsl:template name="print.op.result">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rarrw;</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the ~> (functional dependency) operator -->
  <xsl:template name="print.op.fndep">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rarrw;</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the => implication operator -->
  <xsl:template name="print.op.implies">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rArr;</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the => operator -->
  <xsl:template name="print.op.eval">	
    <xsl:param name="print.op.eval.many"/>
    
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rArr;</xsl:text>    
    <xsl:value-of select="$print.op.eval.many"/>	  
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the =>l operator -->
  <xsl:template name="print.op.leval">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>rAarr;</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the =>r operator -->
  <xsl:template name="print.op.reval">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>xrArr;</xsl:text>    
    <xsl:element name="sup">
      <xsl:text>r</xsl:text>
    </xsl:element>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the :: (cons) operator -->
  <xsl:template name="print.op.cons.bare">	
    <xsl:element name="progident">
      <xsl:text>::</xsl:text>    
    </xsl:element>
  </xsl:template>
  <xsl:template name="print.op.cons">	
    <xsl:call-template name="print.space"/>
    <xsl:call-template name="print.op.cons.bare"/>
    <xsl:call-template name="print.space"/>
  </xsl:template>
  
  <!-- the : (qual) operator -->
  <xsl:template name="print.op.qual.bare">	
    <xsl:text>:</xsl:text>    
  </xsl:template>
  <xsl:template name="print.op.qual">	
    <xsl:call-template name="print.space"/>
    <xsl:call-template name="print.op.qual.bare"/>
    <xsl:call-template name="print.space"/>
  </xsl:template>
  
  <!-- the <: (subQual) operator -->
  <xsl:template name="print.op.subQual">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>le;</xsl:text>
    <xsl:text>:</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the <: (coerce) operator -->
  <xsl:template name="print.op.subqual">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>cupre;</xsl:text>
    <xsl:text>:</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the <|: (Mqual) operator -->
  <xsl:template name="print.op.Mqual">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>ltrie;</xsl:text>
    <xsl:text>:</xsl:text>
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the gen operator -->
  <xsl:template name="print.op.gen">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>cupre;</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the := operator -->
  <xsl:template name="print.op.assign">	
    <xsl:call-template name="print.space"/>
    <xsl:element name="progident">
      <xsl:text>:=</xsl:text>
    </xsl:element>	
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!-- the |->  operator -->
  <xsl:template name="print.op.map">	
    <xsl:call-template name="print.space"/>
    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:text>map;</xsl:text>    
    <xsl:call-template name="print.space"/>
  </xsl:template>

  <!--keywords (texttt font) -->
  <xsl:template name="print.kwd">
    <xsl:param name="print.kwd.kwd"/>
    
    <xsl:element name="progident">
      <xsl:value-of select="$print.kwd.kwd"/>	  
    </xsl:element>
  </xsl:template>

  <!--keywords (texttt font) with space afterwards -->
  <xsl:template name="print.kwds">
    <xsl:param name="print.kwds.kwd"/>

    <xsl:call-template name="print.kwd">
      <xsl:with-param name="print.kwd.kwd">
	<xsl:value-of select="$print.kwds.kwd"/>	        
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="print.space"/>
  </xsl:template>
  
  <!--keywords (texttt font) with surrounding spaces -->
  <xsl:template name="print.skwds">	
    <xsl:param name="print.skwds.kwd"/>

    <xsl:call-template name="print.space"/>
    <xsl:call-template name="print.kwd">
      <xsl:with-param name="print.kwd.kwd">
	<xsl:value-of select="$print.skwds.kwd"/>	        
      </xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.space"/>
  </xsl:template>
   
  <!-- Print structure / union / comp type (wrapper) -->
  <xsl:template name="print.comp">
    <xsl:param name="print.comp.symbol"/>
    
    <xsl:call-template name="print.comp.actual">
      <xsl:with-param name="print.comp.actual.symbol">
	<xsl:value-of select="$print.comp.symbol"/>	  
      </xsl:with-param> 
      <xsl:with-param name="print.comp.actual.name">
	<xsl:value-of select="@name"/>	  
      </xsl:with-param> 
      <xsl:with-param name="print.comp.actual.kind">
	<xsl:value-of select="@kind"/>	  
      </xsl:with-param> 	  
    </xsl:call-template>	
  </xsl:template>
  
  <!-- Actually Print the  structure / union / comp type -->
  <xsl:template name="print.comp.actual">
    <xsl:param name="print.comp.actual.symbol"/>
    <xsl:param name="print.comp.actual.kind"/>
    <xsl:param name="print.comp.actual.name"/>

    <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
    <xsl:value-of select="$print.comp.actual.symbol"/>
    
    <xsl:choose>
      <xsl:when test="$print.comp.actual.kind = 'ref'">
	<xsl:element name="sub">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	  <xsl:text>star;</xsl:text>
	</xsl:element>
      </xsl:when>
      <xsl:when test="$print.comp.actual.kind = 'val'">	
	<xsl:element name="sub">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	  <xsl:text>compfn;</xsl:text>
	</xsl:element>
      </xsl:when>
      <!-- <xsl:otherwise> </xsl:otherwise> -->
    </xsl:choose>    
     
    <xsl:if test="$print.comp.actual.name">
      <xsl:element name="sup">    
	<xsl:value-of select="$print.comp.actual.name"/>
      </xsl:element>    
    </xsl:if>

    <xsl:call-template name="print.params"/>
  </xsl:template>
  
  <!-- Prints a function definition f(x) = y  -->
  <xsl:template name="print.fnxn">
    <xsl:call-template name="print.mathmode">
      <xsl:with-param name="print.mathmode.text">
	<xsl:value-of select="@name"/>
      </xsl:with-param> 
    </xsl:call-template>
    <xsl:call-template name="print.index.dash"/>
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:text>)</xsl:text>
    <xsl:call-template name="print.op.eq"/>
    <xsl:apply-templates select="*[2]" mode="formula"/>	
  </xsl:template>

  <!-- Print Parameters, with paranthesis only if necessary  -->
  <xsl:template name="print.params">
    <xsl:if test = "*">
      <xsl:text>(</xsl:text>      
      <xsl:call-template name="print.children"/>
      <xsl:text>)</xsl:text>      
    </xsl:if> 
  </xsl:template>

  <!-- Print Parameters  -->
  <xsl:template name="print.params_paren">
    <xsl:text>(</xsl:text>      
    <xsl:call-template name="print.children"/>
    <xsl:text>)</xsl:text>      
  </xsl:template>
  
  <!-- Print comma separated list of children  -->
  <xsl:template name="print.children.nbsp">
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() != last()">
	<xsl:call-template name="print.op.comma"/>	

	<xsl:if test="../@break">
	  <xsl:element name="br"/>
	  <xsl:call-template name="print.spaces">
	    <xsl:with-param name="print.spaces.n">
	      <xsl:value-of select="../@break"/>
	    </xsl:with-param> 
	  </xsl:call-template>
	</xsl:if>    
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <!-- Print a list of children  -->
  <xsl:template name="print.children">
    <xsl:param name="print.children.septt"/>     
    <xsl:param name="print.children.sep"/>     
    <xsl:param name="print.children.nosp"/>    
    <xsl:param name="print.children.nbsp"/>     
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() != last()">
	<xsl:choose>
	  <xsl:when test="$print.children.septt">
	    <xsl:element name="progident">
	      <xsl:value-of select="$print.children.septt"/>
	    </xsl:element>
	  </xsl:when>
	  <xsl:when test="$print.children.sep">
	    <xsl:value-of select="$print.children.sep"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="print.op.comma"/>	
	  </xsl:otherwise>
	</xsl:choose>
	<xsl:choose>
	  <xsl:when test="$print.children.nosp">
	  </xsl:when>
	  <xsl:when test="$print.children.nbsp">
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="print.bspace"/>	
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  
  <!-- Print | separated list of children  -->
  <xsl:template name="print.alts">
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() != last()">	
	<xsl:call-template name="print.op.alt"/>	
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <!-- Print type op type -->
  <xsl:template name="print.infix">
    <xsl:param name="print.infix.op"/>
    <xsl:param name="print.infix.sup"/>
    <xsl:param name="print.infix.sub"/>
    <xsl:param name="print.infix.sup.noamp"/>
    <xsl:param name="print.infix.sub.noamp"/>
    <xsl:param name="print.infix.noamp"/>
    <xsl:param name="print.infix.br"/>
    <xsl:param name="print.infix.nosp"/>
    
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>
      <xsl:if test = "position() != last()">	
	<xsl:choose>
	  <xsl:when test="$print.infix.br='yes'">
	    <xsl:call-template name="print.bspace"/>
	  </xsl:when>
	  <xsl:when test="$print.infix.nosp='yes'">
	    <!-- nothing -->
	  </xsl:when> 
	  <xsl:otherwise>
	    <xsl:call-template name="print.space"/>
	  </xsl:otherwise>
	</xsl:choose>
	<xsl:if test="$print.infix.noamp != 'yes'">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	</xsl:if>
	<xsl:value-of select="$print.infix.op"/>
	<xsl:if test="$print.infix.sup != ''">
	  <xsl:element name="sup">
	    <xsl:if test="$print.infix.sup.noamp != 'yes'">
	      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	    </xsl:if>
	    <xsl:value-of select="$print.infix.sup"/>
	  </xsl:element>
	</xsl:if>
	<xsl:if test="$print.infix.sub != ''">
	  <xsl:element name="sub">
	    <xsl:if test="$print.infix.sub.noamp != 'yes'">
	      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	    </xsl:if>
	    <xsl:value-of select="$print.infix.sub"/>
	  </xsl:element>
	</xsl:if>
	<xsl:choose>
	  <xsl:when test="$print.infix.br='yes'">
	    <xsl:call-template name="print.bspace"/>
	  </xsl:when>
	  <xsl:when test="$print.infix.nosp='yes'">
	    <!-- nothing -->
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="print.space"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
    </xsl:for-each>    
  </xsl:template>  

  <!-- Print type op type with implied attributes -->
  <xsl:template name="print.infix.implied">
    <xsl:param name="print.infix.implied.op"/>
	<xsl:call-template name="print.infix">
	  <xsl:with-param name="print.infix.op">
	    <xsl:value-of select="$print.infix.implied.op"/>
	  </xsl:with-param> 
	  <xsl:with-param name="print.infix.br">
	    <xsl:value-of select="@br"/>	
	  </xsl:with-param>
	  <xsl:with-param name="print.infix.nosp">
	    <xsl:value-of select="@nosp"/>
	  </xsl:with-param>
	  <xsl:with-param name="print.infix.sub">
	    <xsl:choose>
	      <xsl:when test="@under='minzT'">
		<xsl:text>dtrif;</xsl:text>
	      </xsl:when>
	      <xsl:when test="@under='minz'">
		<xsl:text>xdtri;</xsl:text>
	      </xsl:when>
	    </xsl:choose>
	  </xsl:with-param>
	</xsl:call-template>    
  </xsl:template>

  <!-- Print type op type -->
  <xsl:template name="print.infix.kwd">
    <xsl:param name="print.infix.kwd.kwd"/>
    
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>
      <xsl:if test = "position() != last()">	
	<xsl:call-template name="print.space"/>
	<xsl:element name="progident">
	  <xsl:value-of select="$print.infix.kwd.kwd"/>
	</xsl:element>
	<xsl:call-template name="print.space"/>
      </xsl:if>
    </xsl:for-each>    
  </xsl:template>  


  <!-- Print type op type -->
  <xsl:template name="print.infix.it">
    <xsl:param name="print.infix.it.kwd"/>
    
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>
      <xsl:if test = "position() != last()">	
	<xsl:call-template name="print.space"/>
	  <xsl:value-of select="$print.infix.it.kwd"/>
	<xsl:call-template name="print.space"/>
      </xsl:if>
    </xsl:for-each>    
  </xsl:template>  
  
  <!-- Print set { .., .., ... } -->
  <xsl:template name="print.set">
    <xsl:text>{</xsl:text>
    <xsl:call-template name="print.children"/>
    <xsl:text>}</xsl:text>
  </xsl:template>  

  <!-- Print special set {| .., .., ... |} -->
  <xsl:template name="print.spset">
    <xsl:text>{|</xsl:text>
    <xsl:call-template name="print.children"/>
    <xsl:text>|}</xsl:text>
  </xsl:template>  
  
  <!-- Print  fnxn(...) -->
  <xsl:template name="print.procedure">
    <xsl:param name="print.procedure.name"/>
    <xsl:value-of select="$print.procedure.name"/>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="print.children"/>
    <xsl:text>)</xsl:text>
  </xsl:template>  

  <!-- Print  maybe types -->
  <xsl:template name="print.maybe">
    <xsl:param name="print.maybe.double"/>
    <xsl:variable name="mbpair1.paren" 
      select="(*[1] = mbpair) or (*[1] = fn) or (*[1] = pair)"/>
    <xsl:variable name="mbpair2.paren" 
      select="(*[2] = mbpair) or (*[2] = fn) or (*[2] = pair)"/>
    <xsl:variable name="mbpair.paren" 
      select="(.. = fn) or (.. = mbpair) or (.. = ref)"/>

    <xsl:if test="$mbpair.paren">
      <xsl:text>(</xsl:text>
    </xsl:if>
    
    <xsl:if test="$mbpair1.paren">
      <xsl:text>(</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="*[1]" mode="formula"/>	
    <xsl:if test="$mbpair1.paren">
      <xsl:text>)</xsl:text>
    </xsl:if>
    
    <xsl:choose>
      <xsl:when test="$print.maybe.double">
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>darr;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	<xsl:text>dharl;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:if test="$mbpair2.paren">
      <xsl:text>(</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="*[2]" mode="formula"/>	
    <xsl:if test="$mbpair2.paren">
      <xsl:text>)</xsl:text>
    </xsl:if>

    <xsl:if test="$mbpair.paren">
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- Print n spaces -->  
  <xsl:template name="print.spaces">
    <xsl:param name="print.spaces.n"/>
    <xsl:if test = "$print.spaces.n &gt; 0">
      <xsl:call-template name="print.space"/>	
      <xsl:call-template name="print.spaces">
	<xsl:with-param name="print.spaces.n">
	  <xsl:value-of select="$print.spaces.n - 1"/>
	</xsl:with-param> 
      </xsl:call-template>
    </xsl:if>
  </xsl:template>  

  <!-- Print Citation -->
  <xsl:template name="print.cite">
    <xsl:element name="xref">
      <xsl:attribute name="ref">
	<xsl:value-of select="@tag"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- Print Citations -->
  <xsl:template name="print.cites">
    <xsl:for-each select="*">
      <xsl:choose>
	<xsl:when test="position() = 1">
	  <!-- nothing -->
	</xsl:when>
	<xsl:when test="position() = last()">
	  <xsl:text> and </xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>, </xsl:text>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="print.cite" select="."/>
    </xsl:for-each>
  </xsl:template>

  <!-- Print Checked Primes (') -->  
  <xsl:template name="print.primed_text">
    <xsl:param name="print.primed_text.text"/>
    <xsl:variable name="print.dash.apos" select='"&apos;"'/>
    <xsl:choose>
      <xsl:when test = "$print.primed_text.text = $print.dash.apos">
	<xsl:element name="sup">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	  <xsl:text>prime;</xsl:text>
	</xsl:element>
      </xsl:when>
      <xsl:when test = "$print.primed_text.text=concat($print.dash.apos,$print.dash.apos)">
	<xsl:element name="sup">
	  <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
	  <xsl:text>Prime;</xsl:text>
	</xsl:element>
      </xsl:when>
      <xsl:otherwise>
	<xsl:element name="sup">
	  <xsl:value-of select="$print.primed_text.text"/>
	</xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Print Dash (t') -->  
  <xsl:template name="print.dash">
    <xsl:if test="@dash">
      <xsl:call-template name="print.primed_text">
	<xsl:with-param name="print.primed_text.text">
	  <xsl:value-of select="@dash"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  
  <!-- Print (Subscripted) Index -->  
  <xsl:template name="print.index">
    <xsl:choose>
      <xsl:when test="@num">
	<xsl:element name="sub">
	  <xsl:value-of select="@num"/>
	  <xsl:if test="@subnum">
	    <xsl:element name="sub">	      
	      <xsl:value-of select="@subnum"/>
	    </xsl:element>
	  </xsl:if>
	</xsl:element>
      </xsl:when>
      <xsl:otherwise>
	<xsl:if test="@subnum">
	  <xsl:text>*** Error: @subnum without @subnum *** </xsl:text>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <!-- Print Index + dash -->  
  <xsl:template name="print.index.dash">
    <xsl:call-template name="print.dash"/>
    <xsl:call-template name="print.index"/>    
  </xsl:template>

  <!-- Print Extensions -->  
  <xsl:template name="print.extn">
    <xsl:for-each select="*">
      <xsl:if test = "position() &gt; 1">
	<xsl:call-template name="print.op.comma"/>	
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>

  <!-- Print Quantifications and Quantified expression -->  
  <xsl:template name="print.quant">
    <xsl:if test = "count(*) = 0">
      <xsl:call-template name="print.space"/>	
    </xsl:if>
    <xsl:for-each select="*">
      <xsl:apply-templates select="." mode="formula"/>	
      <xsl:if test = "position() &lt; (last() - 1)">
	<xsl:call-template name="print.op.comma"/>	
      </xsl:if>
      <xsl:if test = "position() =  last() - 1">
 	<xsl:text>.</xsl:text> 
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <!-- Printing LHS ans RHS of equations -->
  <xsl:template name="print.eqnside">
    <xsl:apply-templates select="*[1]" mode="formula"/>	      
    <!-- dependencies -->
    <xsl:for-each select="*">
      <xsl:if test = "position() &gt; 1">
	<xsl:if test = "position() = 2">
	  <xsl:call-template name="print.space"/>
	  <xsl:text>|</xsl:text>
	  <xsl:call-template name="print.space"/>
	</xsl:if>
	<xsl:if test = "position() &gt; 2">
	  <xsl:text>,</xsl:text>
	  <xsl:call-template name="print.space"/>	
	</xsl:if>	    
	<xsl:apply-templates select="." mode="formula"/>	
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  
  <!-- Print Premices -->
  <xsl:template name="print.premices">
    <xsl:for-each select="*">
      <xsl:if test = "position() &gt; 1">
	<xsl:call-template name="print.space"/>	
	<xsl:call-template name="print.space"/>	
	<xsl:call-template name="print.space"/>	
	<xsl:call-template name="print.space"/>	
      </xsl:if>
      <xsl:apply-templates select="." mode="formula"/>	
    </xsl:for-each>
  </xsl:template>
  
  <!-- Print Logic Rule:
           Premices
           Premices
       ________________
          Conclusion    -->
  <xsl:template name="print.rule">
    <xsl:element name="table">
      <xsl:element name="tbody">
	<!-- OpRule Name -->
	<xsl:if test="@name">
	  <xsl:element name="tr">
	    <xsl:attribute name="valign">top</xsl:attribute>
	    <xsl:element name="td">
	      <xsl:element name="p">		
		<xsl:element name="b">		
		  <xsl:value-of select="@name"/>
		</xsl:element>
	      </xsl:element>
	    </xsl:element>
	  </xsl:element>
	</xsl:if>
	<!-- Premises, Conclusion -->
	<xsl:for-each select="*">
	  <xsl:element name="tr">
	    <xsl:attribute name="valign">top</xsl:attribute>
	    <xsl:if test = "position() = last() - 1">
	      <xsl:attribute name="lineafter">yes</xsl:attribute>
	    </xsl:if>
	    <xsl:element name="td">
	      <xsl:attribute name="align">center</xsl:attribute>
	      <xsl:element name="p">
		<xsl:apply-templates select="." mode="formula"/>	
	      </xsl:element>
	    </xsl:element>
	  </xsl:element>
	</xsl:for-each>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <!-- Print Logic Rule:
          Premices
          Premices
   Name  ________________
           Conclusion    -->
  <xsl:template name="print.rule.2">
    <xsl:element name="table">
      <xsl:element name="tbody">
	<xsl:element name="tr">	  
	  <xsl:attribute name="valign">top</xsl:attribute>
	  <!-- OpRule Name -->
	  <xsl:if test="@name">
	    <xsl:element name="td">
	      <xsl:element name="p">		
		<!-- <xsl:element name="b">		
		<xsl:value-of select="@name"/>
	      </xsl:element> -->
		<xsl:text>S1</xsl:text>
	      </xsl:element>
	    </xsl:element>
	  </xsl:if>
	  <xsl:element name="td">
	    <xsl:element name="table">
	      <xsl:element name="tbody">
		<!-- Premises, Conclusion -->
		<xsl:for-each select="*">
		  <xsl:element name="tr">
		    <xsl:attribute name="valign">top</xsl:attribute>
		    <xsl:if test = "position() = last() - 1">
		      <xsl:attribute name="lineafter">yes</xsl:attribute>
		    </xsl:if>
		    <xsl:element name="td">
		      <xsl:attribute name="align">center</xsl:attribute>
		      <xsl:element name="p">
			<xsl:apply-templates select="." mode="formula"/>	
		      </xsl:element>
		    </xsl:element>
		  </xsl:element>
		</xsl:for-each>
	      </xsl:element>
	    </xsl:element>
	  </xsl:element>
	</xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
  
<!-- ======================================================================
      ======================================================================
                                Depricated
      ======================================================================
      ====================================================================== -->

  <xsl:template match="hset" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="hunion" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="hUnion" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="hIn" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  

  <xsl:template match="ctset" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="ctunion" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="ctUnion" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="ctIn" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  

  <xsl:template match="lnset" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="lnunion" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="lnUnion" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="lnIn" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  

  <xsl:template match="tyset" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="tyunion" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="tyUnion" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="tyIn" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="tydiff" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  

  <xsl:template match="valMap" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="typeMap" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
 
  <xsl:template match="extStack" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="extHeap" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="extGamma" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  <xsl:template match="extStore" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  

  <xsl:template match="eq_ct" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>     
  <xsl:template match="ceq_ct" mode="formula">
    <xsl:call-template name="print.depricated"/>
  </xsl:template>  
  
  <!-- Warn Depricated -->
  <xsl:template name="print.depricated">
    <xsl:element name="em">
      <xsl:element name="b">
	<xsl:text>DEPRICATED</xsl:text>
      </xsl:element>
    </xsl:element>
  </xsl:template>  
  
</xsl:stylesheet>
