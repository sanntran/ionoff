package net.ionoff.center.client.base;

import com.google.gwt.event.shared.HandlerManager;

import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;

public abstract class AbstractEditPresenter<T extends BaseDto> extends AbstractPresenter {
	
	protected boolean isDirty;
	protected IEditView<T> display;
	protected IRpcServiceProvider rpcProvider;

	public AbstractEditPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, IEditView<T> view) {
		super(eventBus);
		this.rpcProvider = rpcProvider;
		this.display = view;
	}

	protected abstract void save();

	protected abstract String getClazz();
	
	protected abstract EntityService<T> getRpcService();


	protected void bind() {

	}

	protected boolean validateInputNumberValue(String field, String value) {
		if (value == null || value.trim().isEmpty()) {
			final String message = AdminLocale.getAdminMessages().emptyInputValue(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		if (!ClientUtil.isIntNumber(value)) {
			final String message = AdminLocale.getAdminMessages().invalidNumberValue(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		return true;
	}

	protected boolean validateInputStringValue(String field, String value) {
		if (value == null || value.trim().isEmpty()) {
			final String message = AdminLocale.getAdminMessages().emptyInputValue(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		if (value.length() > 250) {
			final String message = AdminLocale.getAdminMessages().overMaximunLength(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		if (value.contains("#")) {
			final String message = AdminLocale.getAdminMessages().containsSpecialCharacters(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		return true;
	}
}
