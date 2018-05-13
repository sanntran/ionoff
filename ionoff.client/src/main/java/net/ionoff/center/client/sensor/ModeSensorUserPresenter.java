package net.ionoff.center.client.sensor;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.ModeSensorUserDto;

public class ModeSensorUserPresenter extends AbstractPresenter {
	
	public interface Display  {
		Widget asWidget();
		Label getLblUserName();
		CheckBox getCheckBoxSendSms();
		CheckBox getCheckBoxSendEmail();
	}
	
	private final IRpcServiceProvider rpcProvider;
	private final Display display;
	private ModeSensorUserDto modeSensorUser;
	
	public ModeSensorUserPresenter(ModeSensorUserDto modeSensorUser, IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(eventBus);
		this.rpcProvider= rpcProvider;
		this.display = view;
		this.modeSensorUser = modeSensorUser;
	}
	
	@Override
	public void go() {
		bind();
	}

	private void bind() {
		display.getCheckBoxSendSms().addValueChangeHandler(new ValueChangeHandler<Boolean>() {
			@Override
			public void onValueChange(ValueChangeEvent<Boolean> event) {
				onChangeSensorUser();
			}
		});
		display.getCheckBoxSendEmail().addValueChangeHandler(new ValueChangeHandler<Boolean>() {
			@Override
			public void onValueChange(ValueChangeEvent<Boolean> event) {
				onChangeSensorUser();
			}
		});
	}

	private void onChangeSensorUser() {
		modeSensorUser.setSendSms(display.getCheckBoxSendSms().getValue());
		modeSensorUser.setSendEmail(display.getCheckBoxSendEmail().getValue());
		rpcProvider.getModeSensorUserService().update(modeSensorUser.getId(), modeSensorUser, 
				new MethodCallback<ModeSensorUserDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ModeSensorUserDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
				display.getCheckBoxSendEmail().setValue(modeSensorUser.isSendEmail());
				display.getCheckBoxSendSms().setValue(modeSensorUser.isSendSms());
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		// does nothing
	}
}
