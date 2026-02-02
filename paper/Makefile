all: paper.pdf

PAPER_FIGURES := figures/counterfactual_maps.pdf \
                 figures/gravity_binscatter_residualized.pdf

paper.pdf: paper.tex sections/*.tex figures/*.pdf $(PAPER_FIGURES) spatial_hw1.bib
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make paper.tex

figures/counterfactual_maps.pdf: ../tasks/redline_counterfactual/output/counterfactual_maps.pdf | figures
	ln -sf ../../tasks/redline_counterfactual/output/counterfactual_maps.pdf $@

figures/gravity_binscatter_residualized.pdf: ../tasks/estimate_gravity_travel_time/output/gravity_binscatter_residualized.pdf | figures
	ln -sf ../../tasks/estimate_gravity_travel_time/output/gravity_binscatter_residualized.pdf $@

figures:
	mkdir -p figures

clean:
	latexmk -CA

.PHONY: all clean
