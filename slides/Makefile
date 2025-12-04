FONT_DIR = fonts
CODE_FONT_URL = https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/CascadiaCode.zip
CODE_FONT_ZIP = $(FONT_DIR)/CascadiaCode.zip
CODE_FONTS = $(FONT_DIR)/CaskaydiaCoveNerdFont*.ttf

all: main.pdf

main.pdf: main.typ $(FONT_DIR)/.fonts-extracted
	typst compile --font-path $(FONT_DIR) main.typ

intro.pdf: intro.typ $(FONT_DIR)/.fonts-extracted forklift.jpg
	typst compile --font-path $(FONT_DIR) intro.typ

forklift.jpg:
	mkdir -p images
	wget -O images/forklift.jpg https://i.redd.it/hor72ca6g55g1.jpeg

$(FONT_DIR)/.fonts-extracted: $(CODE_FONT_ZIP)
	unzip $(CODE_FONT_ZIP) "*.ttf" -d $(FONT_DIR)
	touch $@

$(CODE_FONT_ZIP):
	mkdir -p $(FONT_DIR)
	wget -O $(CODE_FONT_ZIP) $(CODE_FONT_URL)

clean:
	rm -rf main.pdf $(FONT_DIR)
