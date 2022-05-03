export PRE="Proyecto"
export NUM="00"
buf_size=10000000 xelatex --shell-escape doc.tex \
  && rm -rf _minted-* \
  && rm -rf *.{aux,log,out,fdb_latexmk,fls,synctex.gz} \
  && mv doc.pdf ${PRE}_${NUM}_Iván\ Molina\ Rebolledo.pdf \
  && open ${PRE}_${NUM}_Iván\ Molina\ Rebolledo.pdf
