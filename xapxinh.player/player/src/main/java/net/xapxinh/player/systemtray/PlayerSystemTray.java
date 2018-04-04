package net.xapxinh.player.systemtray;

import java.awt.AWTException;
import java.awt.Image;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;

import javax.swing.ImageIcon;
import javax.swing.SwingUtilities;

import org.apache.log4j.Logger;

import net.xapxinh.player.AppProperties;
import net.xapxinh.player.connection.TcpConnection;
import net.xapxinh.player.hardware.Hardware4Win;

public class PlayerSystemTray extends Thread {

	private static final Logger logger = Logger.getLogger(PlayerSystemTray.class.getSimpleName());

	private TrayIcon trayIcon;
	private TcpConnection tcpConnection;
	private final SystemTray systemTray;

	public PlayerSystemTray(TcpConnection tcpConnection) {
		this.tcpConnection = tcpConnection;
		systemTray = SystemTray.getSystemTray();
	}
	
	@Override
	public void run() {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				try {
					createAndShowGUI();
				}
				catch (final Exception e) {
					logger.error(e.getMessage(), e);
				}
			}
		});
	}

	private void createAndShowGUI() throws Exception {
		initTrayIcon();
		final PopupMenu popupMenu = new SystemTrayPopupMenu(this);
		trayIcon.setPopupMenu(popupMenu);
		if (!SystemTray.isSupported()) {
			logger.error("SystemTray is not supported");
			return;
		}
		try {
			systemTray.add(trayIcon);
		}
		catch (final AWTException e) {
			logger.error(AppProperties.getAppName() + " could not be added.", e);
			return;
		}
	}

	private void initTrayIcon() {
		trayIcon = new TrayIcon(createSystemTrayIcon("icon_16.png", "tray icon"), AppProperties.getAppName());
		trayIcon.setImageAutoSize(true);

		trayIcon.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				String mac = "";
				try {
					mac = Hardware4Win.getMotherboardSN();
				}
				catch (final Exception ex) {
					logger.error("Error getting phisical MAC", ex);
				}
				showInfoMessage("ID: " + AppProperties.getPlayerName() + " Password: " + AppProperties.getRandomPassword() + " MAC: " + mac);
			}
		});
	}

	private Image createSystemTrayIcon(final String path, final String description) {
		final URL imageURL = getClass().getResource("/" + path);
		if (imageURL == null) {
			return null;
		}
		return new ImageIcon(imageURL, description).getImage();
	}

	public TrayIcon getTrayIcon() {
		return trayIcon;
	}

	private void showInfoMessage(String message) {
		String title = AppProperties.getAppName() + " - " + tcpConnection.getStatus();
		if (tcpConnection.isConected()) {
			trayIcon.displayMessage(title, message, TrayIcon.MessageType.INFO);
		}
		else {
			trayIcon.displayMessage(title, message, TrayIcon.MessageType.WARNING);
		}
	}

	public void finish() {
		systemTray.remove(trayIcon);
	}
}