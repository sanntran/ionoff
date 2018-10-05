package net.ionoff.center.server.locale;

import java.text.MessageFormat;
import java.util.Date;
import java.util.Locale;
import java.util.ResourceBundle;

import net.ionoff.center.server.mediaplayer.locale.UTF8Control;

/**
 * @author Sann Tran
 */
public class Messages {
	private static Messages viMessages;
	private static Messages enMessages;

	private final ResourceBundle resourceBundle;
	private final MessageFormat messageFormat;

	private static final ResourceBundle VI_VN_BUNDLE = ResourceBundle.getBundle("Messages", new Locale("vi", "VN"), new UTF8Control());
	private static final ResourceBundle EN_US_BUNDLE = ResourceBundle.getBundle("Messages", new Locale("en", "US"), new UTF8Control());

	private Messages(ResourceBundle resourceBundle) {
		this.resourceBundle = resourceBundle;
		messageFormat = new MessageFormat("");
	}

	public static Messages get(String locale) {
		if ("en".equals(locale)) {
			if (enMessages == null) {
				enMessages = new Messages(EN_US_BUNDLE);
			}
			return enMessages;
		}
		else {
			if (viMessages == null) {
				viMessages = new Messages(VI_VN_BUNDLE);
			}
			return viMessages;
		}
	}

	public String invalidSymbol(String symbol) {
		messageFormat.applyPattern(resourceBundle.getString("invalidSymbol"));
		return messageFormat.format(new Object[] {symbol});
	}

	public String lastUpdate(Date timestamp) {
		messageFormat.applyPattern(resourceBundle.getString("lastUpdate"));
		return messageFormat.format(new Object[] {timestamp});
	}

	public String fieldInvalid(String name, String value) {
		messageFormat.applyPattern(resourceBundle.getString("fieldInvalid"));
		return messageFormat.format(new Object[] {name, value});
	}

	public String invalidParam(String paramName, String paramValue) {
		messageFormat.applyPattern(resourceBundle.getString("invalidParam"));
		return messageFormat.format(new Object[] {paramName, paramValue});
	}

	public String unknownRpcCommand(String param) {
		messageFormat.applyPattern(resourceBundle.getString("unknownRpcCommand"));
		return messageFormat.format(new Object[] {param});
	}

	public String invalidHost(String host) {
		messageFormat.applyPattern(resourceBundle.getString("invalidHost"));
		return messageFormat.format(new Object[] {host});
	}

	public String unknownRPCClazz(String rpcClazz) {
		messageFormat.applyPattern(resourceBundle.getString("unknownRPCClazz"));
		return messageFormat.format(new Object[] {rpcClazz});
	}

	public String errorDeleteSingleEntity() {
		return resourceBundle.getString("errorDeleteSingleEntity");
	}

	public String userNameIsDuplicated(String userName) {
		messageFormat.applyPattern(resourceBundle.getString("userNameIsDuplicated"));
		return messageFormat.format(new Object[] {userName});
	}

	public String errorChangeSuperAdminUserName(String userName) {
		messageFormat.applyPattern(resourceBundle.getString("errorChangeSuperAdminUserName"));
		return messageFormat.format(new Object[] {userName});
	}

	public String errorDeleteNotEmptyEntity(String entity, String ownedEntity) {
		messageFormat.applyPattern(resourceBundle.getString("errorDeleteNotEmptyEntity"));
		return messageFormat.format(new Object[] {entity, ownedEntity});
	}

	public String errorSensorDriverIndex() {
		return resourceBundle.getString("errorSensorDriverIndex");
	}

	public String errorSetRelayTypeButtonForLight() {
		return resourceBundle.getString("errorSetRelayTypeButtonForLight");
	}

	public String errorSetManyRelayForLight() {
		return resourceBundle.getString("errorSetManyRelayForLight");
	}

	public String relayDriverIpDuplicated(String relayDriverIp) {
		messageFormat.applyPattern(resourceBundle.getString("relayDriverIpDuplicated"));
		return messageFormat.format(new Object[] {relayDriverIp});
	}

	public String errorConnectRelayDriver(String relayDriver) {
		messageFormat.applyPattern(resourceBundle.getString("errorConnectRelayDriver"));
		return messageFormat.format(new Object[] {relayDriver});
	}

	public String errorSetRelayForPlayer() {
		return resourceBundle.getString("errorSetRelayForPlayer");
	}

	public String permissionDenied() {
		return resourceBundle.getString("permissionDenied");
	}

	public String requestUnauthorized() {
		return resourceBundle.getString("requestUnauthorized");
	}

	public String applicationUnactivated() {
		return resourceBundle.getString("applicationUnactivated");
	}

	public String unknownRelayDriverModel(String relayDriverModel) {
		messageFormat.applyPattern(resourceBundle.getString("unknownRelayDriverModel"));
		return messageFormat.format(new Object[] {relayDriverModel});
	}

	public String resourceIdNotFound(String resourceName, Long resourceId) {
		messageFormat.applyPattern(resourceBundle.getString("resourceIdNotFound"));
		return messageFormat.format(new Object[] {resourceName, resourceId});
	}

	public String notAllowChangingRoleOfAdminZone() {
		return resourceBundle.getString("notAllowChangingRoleOfAdminZone");
	}

	public String errorDeleteUserLord() {
		return resourceBundle.getString("errorDeleteUserLord");
	}
	
	public String errorUserIsNotSystemAdmin() {
		return resourceBundle.getString("errorUserIsNotSystemAdmin");
	}

	public String errorRelayLocked(String relayName) {
		messageFormat.applyPattern(resourceBundle.getString("errorRelayLocked"));
		return messageFormat.format(new Object[] {relayName});
	}
	
	public String lordHaveToBelongToAproject() {
		return resourceBundle.getString("lordHaveToBelongToAproject");
	}
	
}
