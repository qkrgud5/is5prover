
let template s1 s2 s3 = Printf.sprintf
"
\\documentclass{article}
\\usepackage[a2paper, landscape]{geometry}
\\usepackage{datetime}
\\usepackage{proof, amsmath, amsthm, amssymb}
\\usepackage{fullpage}
\\usepackage{color}

\\newcommand{\\seq}[4]{#1 \\vdash #2 \\vdash #3 \\vdash #4}
\\newcommand{\\seqc}[4]{G #1 \\vdash \\Delta #2 \\vdash \\Gamma #3 \\vdash #4}

\\begin{document}

\\currenttime

%s

%s

%s
\\end{document} 
"
s1 s2 s3;;
