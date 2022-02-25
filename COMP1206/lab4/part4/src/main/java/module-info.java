module uk.ac.soton.comp1206 {
    requires java.scripting;
    requires javafx.controls;
    requires javafx.fxml;
    requires javafx.media;
    requires javafx.swing;
    requires java.desktop;
    requires javafx.base;
    requires org.apache.logging.log4j;
    requires nv.websocket.client;
    opens uk.ac.soton.comp1206.ui to javafx.fxml;
    opens uk.ac.soton.comp1206.utility to javafx.fxml;
    exports uk.ac.soton.comp1206;
    exports uk.ac.soton.comp1206.ui;
    exports uk.ac.soton.comp1206.network;
    exports uk.ac.soton.comp1206.utility;
    exports uk.ac.soton.comp1206.drawing;
}
