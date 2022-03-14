<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <html>
            <body>
                <h2>Pizza Menu</h2>
                <table border="9acd32">
                    <tr>
                        <th style="text-align:left">Pizza Name</th>
                        <th style="text-align:left">Toppings</th>
                    </tr>
                    <xsl:for-each select="Pizzas/Pizza">
                        <tr>
                            <td><xsl:value-of select="@name"/></td>
                            <td>
                                <ul>
                                    <xsl:for-each select="Toppings/Topping">
                                        <li><xsl:value-of select="text()"/></li>
                                    </xsl:for-each>
                                </ul>
                            </td>
                        </tr>
                    </xsl:for-each>
                </table>
            </body>
        </html>
    </xsl:template>
</xsl:stylesheet>