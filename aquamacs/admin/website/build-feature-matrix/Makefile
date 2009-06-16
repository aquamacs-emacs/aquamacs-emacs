SRC = features.py
HTML = features.html
PYSRC = Category.py FeatureMatrix.py ValueMatrix.py
INSTALL_TO = jones.tc:jones.tc/other
IMAGE_DIR = images
IMAGES = $(IMAGE_DIR)/minus.png $(IMAGE_DIR)/plus.jpg $(IMAGE_DIR)/minus.png

$(HTML): $(SRC) $(PYSRC) $(IMAGES)
	python $< > $@

clean:
	rm -f $(HTML)

install: $(HTML) $(IMAGES)
	scp $(HTML) $(INSTALL_TO)
	scp $(IMAGES) $(INSTALL_TO)/$(IMAGE_DIR)

tar:
	tar cfvj features.tar.bz2 $(SRC) $(PYSRC) $(IMAGES) Makefile 
