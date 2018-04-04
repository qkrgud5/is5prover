
let template s1 s2 s3 = Printf.sprintf
"
\\documentclass{article}
\\usepackage[a2paper, landscape]{geometry}
\\usepackage{datetime}
\\usepackage{proof, amsmath, amsthm, amssymb, latexsym}
\\usepackage{fullpage}
\\usepackage{color}

\\newcommand{\\seq}[4]{#1 \\vdash #2 \\vdash #3 \\longrightarrow #4}
\\newcommand{\\seqc}[4]{\\Sigma #1 \\vdash \\Delta #2 \\vdash \\Gamma #3 \\longrightarrow #4}

\\begin{document}

\\currenttime

%s

%s

%s
\\end{document} 
"
s1 s2 s3;;
