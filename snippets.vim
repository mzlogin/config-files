" Templates: {{{1
" to add templates for new file type, see below
"
" "some new file type
" let g:template['newft'] = {}
" let g:template['newft']['keyword'] = "some abbrevation"
" let g:template['newft']['anotherkeyword'] = "another abbrevation"
" ...
"
" ---------------------------------------------
" common templates
let g:template['_'] = {}
let g:template['_']['date'] = "\<c-r>=strftime(\"%Y-%m-%d\")\<cr>"
let g:template['_']['time'] = "\<c-r>=strftime(\"%H:%M:%S\")\<cr>"
let g:template['_']['datetime'] = "\<c-r>=strftime(\"%Y-%m-%d %H:%M:%S\")\<cr>"

" ---------------------------------------------
" C templates
let g:template['c'] = {}
let g:template['c']['cc'] = "/*  */\<left>\<left>\<left>"
let g:template['c']['cd'] = "/**  */\<left>\<left>\<left>"
let g:template['c']['de'] = "#define "
let g:template['c']['in'] = "#include \"\"\<left>"
let g:template['c']['is'] = "#include <>\<left>"
let g:template['c']['ff'] = "#ifndef \<c-r>=GetFileName()\<cr>\<CR>#define \<c-r>=GetFileName()\<cr>".
            \repeat("\<cr>",3)."#endif // \<c-r>=GetFileName()\<cr>"
let g:template['c']['for'] = "for (".g:rs."...".g:re."; ".g:rs."...".g:re."; ".g:rs."...".g:re.") {\<cr>".
            \g:rs."...".g:re."\<cr>}\<cr>"
let g:template['c']['main'] = "int main(int argc, char \*argv\[\]) {\<cr>".g:rs."...".g:re.";\<cr>\<cr>return 0;\<cr>}"
let g:template['c']['switch'] = "switch (".g:rs."...".g:re.") {\<cr>case ".g:rs."...".g:re.":\<cr>break;\<cr>case ".
            \g:rs."...".g:re.":\<cr>break;\<cr>default:\<cr>break;\<cr>}"
let g:template['c']['if'] = "if (".g:rs."...".g:re.") {\<cr>".g:rs."...".g:re."\<cr>}"
let g:template['c']['while'] = "while (".g:rs."...".g:re.") {\<cr>".g:rs."...".g:re."\<cr>}"
let g:template['c']['ife'] = "if (".g:rs."...".g:re.") {\<cr>".g:rs."...".g:re."\<cr>} else {\<cr>".g:rs."...".
            \g:re."\<cr>}"
let g:template['c']['elif'] = "else if (".g:rs."...".g:re.") {\<cr>".g:rs."...".g:re."\<cr>}"
let g:template['c']['header'] = "/**\<cr>Source : \<c-r>=GetFileName()\<cr>\<cr>Author : ".g:author_for_snippets."\<cr>Email  : ".g:email_for_snippets."\<cr>Date   : \<c-r>=strftime(\"%Y-%m-%d\")\<cr>\<cr>*/"

" ---------------------------------------------
" C++ templates
let g:template['cpp'] = g:template['c']
let g:template['cpp']['uns'] = "using namespace std;"

" ---------------------------------------------
" Python templates
let g:template['python'] = {}
let g:template['python']['header'] = "#-*- encoding: utf-8 -*-\<cr>"
let g:template['python']['ifmain'] = "def main():\<cr>pass\<cr>\<cr>if __name__ == '__main__':\<cr>main()"

" ---------------------------------------------
" Java templates
let g:template['java'] = {}
let g:template['java']['psvm'] = "public static void main(String[] args) {\<cr>".g:rs."...".g:re."\<cr>}"
let g:template['java']['sop'] = 'System.out.println('

" ---------------------------------------------
" git commit msg
let g:template['gitcommit'] = {}
let g:template['gitcommit']['cs'] = "--story=".g:rs."...".g:re." --user=".g:user_for_snippets." "
let g:template['gitcommit']['cb'] = "--bug=".g:rs."...".g:re." --user=".g:user_for_snippets." "
let g:template['gitcommit']['ct'] = "--task=".g:rs."...".g:re." --user=".g:user_for_snippets." "
let g:template['gitcommit']['cr'] = "ref ".g:rs."...".g:re
