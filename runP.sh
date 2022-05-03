export PRE="Proyecto"
export NUM="Presentación"
buf_size=10000000 xelatex --shell-escape presentación.tex \
  && rm -rf _minted-* \
  && rm -rf *.{aux,log,out,fdb_latexmk,fls,synctex.gz} \
  && mv presentación.pdf ${PRE}_${NUM}_Iván\ Molina\ Rebolledo.pdf \
  && open ${PRE}_${NUM}_Iván\ Molina\ Rebolledo.pdf
