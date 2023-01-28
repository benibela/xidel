xidel examples
==============

Run as e.g.:
```bash
xidel --html --xquery3="$(cat examples/1.xq)"
```

Which in this case yields:
```html
<!DOCTYPE html>
<table>
<tbody>
<tr class="C_123 hascol1" id="123">
  <td>123</td><!--  -->
</tr>
<tr class="C_456 C_other hascol1 hascomment" id="456">
  <td>456</td><!-- other -->
</tr>
<tr class="C_foo C_columns hascol1 hascomment" id="foo">
  <td>foo</td><!-- columns -->
</tr>
<tr class="C_bar C_are hascol1 hascomment" id="bar">
  <td>bar</td><!-- are -->
</tr>
<tr class="C_xyz C_ignored C_zomg hascol1 hascomment zomg" id="xyz">
  <td name="zomg">xyz</td><!-- ignored -->
</tr>
</tbody>
</table>
```
