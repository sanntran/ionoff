package net.xapxinh.player.systemtray;

import java.io.File;

import net.jimmc.jshortcut.JShellLink;
import net.xapxinh.player.AppProperties;

import org.apache.log4j.Logger;

public final class ShortcutCreation {
	private static final Logger logger = Logger.getLogger(ShortcutCreation.class
			.getSimpleName());
	private static ShortcutCreation instance;
	
	public void createShortcut() {
		try {
			JShellLink link = new JShellLink();
			String filePath = AppProperties.getExePath();			
			link.setFolder(new File("").getAbsolutePath());
			link.setName("xmp.exe");
			link.setPath(filePath);
			link.save();
		}
		catch (Exception ex) {
			logger.error(ex.getMessage(), ex);
		}
	}
	
	public static ShortcutCreation getInstance() {
		if (instance == null) {
			instance =  new ShortcutCreation();
		}
		return instance;
	}
}