<xml>
<meth><?php echo $_SERVER['REQUEST_METHOD']?></meth>
<raw><?php echo file_get_contents("php://input"); ?></raw>
</xml>
