package net.xapxinh.player.systemtray;

import java.awt.CheckboxMenuItem;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import net.xapxinh.player.AppProperties;
import net.xapxinh.player.Application;

public class SystemTrayPopupMenu extends PopupMenu {
	
	private static final long serialVersionUID = 1L;

	private static final String RUN_ON_STARTUP = "Run on Startup";
	private static final String EXIT = "Exit";
	
	private final CheckboxMenuItem runOnStartupMenuItem;
	private final MenuItem exitMenuItem;
	
	private PlayerSystemTray systemTray;
	
	SystemTrayPopupMenu(PlayerSystemTray systemTray) {
		this.systemTray = systemTray;
		runOnStartupMenuItem = new CheckboxMenuItem(RUN_ON_STARTUP);
		if (isRunOnStartup()) {
			runOnStartupMenuItem.setState(true);
		}
		else {
			runOnStartupMenuItem.setState(false);
		}
		add(runOnStartupMenuItem);		
		addSeparator();
		exitMenuItem = new MenuItem(EXIT);
		add(exitMenuItem);
		addActionListeners();
	}
	
	private boolean isRunOnStartup() {
		return WindowsReqistry.readRegistry(WindowsReqistry.START_UP_LOCATION, WindowsReqistry.XMP_VALUE).contains(WindowsReqistry.XMP_VALUE);
	}

	private void addActionListeners() {
		runOnStartupMenuItem.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(final ItemEvent e) {
				final int stateChange = e.getStateChange();
				if (stateChange == ItemEvent.SELECTED) {
					ShortcutCreation.getInstance().createShortcut();
					WindowsReqistry.addValue(WindowsReqistry.START_UP_LOCATION, 
							WindowsReqistry.XMP_VALUE, AppProperties.getExeShorcutPath());
				}
				else {
					WindowsReqistry.deleteValue(WindowsReqistry.START_UP_LOCATION, WindowsReqistry.XMP_VALUE);
				}
			}
		});
		
		exitMenuItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				systemTray.finish();
				Application.application().quit();
				System.exit(0);
			}
		});
	}
}
