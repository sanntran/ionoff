package net.xapxinh.player.view.main;

import static net.xapxinh.player.Application.application;

import java.awt.CardLayout;

import javax.swing.JPanel;

import net.xapxinh.player.view.image.ImagePane;

@SuppressWarnings("serial")
final class VideoContentPane extends JPanel {

    private static final String NAME_DEFAULT = "default";

    private static final String NAME_VIDEO = "video";

    private final CardLayout cardLayout;

    VideoContentPane() {
        cardLayout = new CardLayout();
        setLayout(cardLayout);
        add(new ImagePane(ImagePane.Mode.CENTER, getClass().getResource("/logo.png"), 0.3f), NAME_DEFAULT);
        add(application().mediaPlayerPanel(), NAME_VIDEO);
    }

    public void showDefault() {
        cardLayout.show(this, NAME_DEFAULT);
    }

    public void showVideo() {
        cardLayout.show(this, NAME_VIDEO);
    }
}
