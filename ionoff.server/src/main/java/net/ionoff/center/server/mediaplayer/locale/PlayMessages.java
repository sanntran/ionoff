package net.ionoff.center.server.mediaplayer.locale;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.ResourceBundle;

/**
 * @author Sann Tran
 */
public class PlayMessages {

	private static PlayMessages viMessages;
	private static PlayMessages enMessages;

	private final ResourceBundle resourceBundle;
	private final MessageFormat messageFormat;

	private static final ResourceBundle VI_VN_BUNDLE = ResourceBundle.getBundle("PlayMessages", new Locale("vi", "VN"), new UTF8Control());
	private static final ResourceBundle EN_US_BUNDLE = ResourceBundle.getBundle("PlayMessages", new Locale("en", "US"), new UTF8Control());

	private PlayMessages(ResourceBundle resourceBundle) {
		this.resourceBundle = resourceBundle;
		messageFormat = new MessageFormat("");
	}

	public static PlayMessages get(String locale) {
		if ("en".equals(locale)) {
			if (enMessages == null) {
				enMessages = new PlayMessages(EN_US_BUNDLE);
			}
			return enMessages;
		}
		else {
			if (viMessages == null) {
				viMessages = new PlayMessages(VI_VN_BUNDLE);
			}
			return viMessages;
		}
	}
	public String playerIsUnknown(String playerId) {
		messageFormat.applyPattern(resourceBundle.getString("playerIsUnknown"));
		return messageFormat.format(new Object[] {playerId});
	}
	public String playerConnectionError() {
		return resourceBundle.getString("playerConnectionError");
	}
	public String dataServiceConnecttionError() {
		return resourceBundle.getString("dataServiceConnecttionError");
	}
}
