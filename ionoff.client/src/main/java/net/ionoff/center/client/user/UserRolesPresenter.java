package net.ionoff.center.client.user;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialCollection;
import net.ionoff.center.client.common.AbstractPresenter;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.UserDeviceDto;
import net.ionoff.center.shared.dto.UserDto;
import net.ionoff.center.shared.dto.UserProjectDto;
import net.ionoff.center.shared.dto.UserSceneDto;
import net.ionoff.center.shared.dto.UserZoneDto;

public class UserRolesPresenter extends AbstractPresenter {
	
	private UserDto user;
	
	public interface Display {
		FlowPanel asPanel();
		FlowPanel getContentPanel();
	}
	
	private final Display display;
	private final HandlerManager eventBus;
	private final IRpcServiceProvider rpcProvider;
	private final List<UserZoneDto> userZones;
	
	public UserRolesPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(eventBus);
		
		this.display = view;
		this.rpcProvider = rpcProvider;
		this.eventBus = eventBus;
		this.userZones = new ArrayList<>();
	}

	@Override
	public void go() {
		bind();
	}

	private void bind() {
		
	}

	public void setUser(UserDto userDto) {
		user = userDto;
		if (userDto.isNew()) {
			display.asPanel().setVisible(false);
			return;
		}
		else {
			display.asPanel().setVisible(true);
		}
		display.getContentPanel().clear();
		if (AppToken.hasTokenItem(AppToken.SYSTEM)) {
			rpcGetUserProjects();
		}
		else {
			rpcGetUserZonesByProjectId();
		}
	}

	private void rpcGetUserProjects() {
		rpcProvider.getUserService().getUserProjectsByUser(user.getId(), new MethodCallback<List<UserProjectDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
				showUserProjects(Collections.emptyList());
			}

			@Override
			public void onSuccess(Method method, List<UserProjectDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				display.getContentPanel().clear();
				showUserProjects(result);
			}
		});
	}

	private void showUserProjects(List<UserProjectDto> userProjects) {
		MaterialCollection userProjectCollection = new MaterialCollection();
		
		for (UserProjectDto userProject : userProjects) {
			UserProjectView userProjectView = new UserProjectView(userProject);
			userProjectCollection.add(userProjectView);
			
			userProjectView.getCheckBoxRole().addValueChangeHandler(new ValueChangeHandler<Boolean>() {
				
				@Override
				public void onValueChange(ValueChangeEvent<Boolean> event) {
					userProject.setRole(userProjectView.getCheckBoxRole().getValue());
					rpcUpdateUserProject(userProject);
				}
			});
		}
		display.getContentPanel().clear();
		display.getContentPanel().add(userProjectCollection);
	}

	private void rpcUpdateUserProject(UserProjectDto userProject) {
		rpcProvider.getUserService().updateUserProject(userProject.getId(), userProject, new MethodCallback<UserProjectDto>() {

			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, UserProjectDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
			}
		});
	}

	private void rpcGetUserZonesByProjectId() {
		rpcProvider.getUserService().getUserZonesByProjectId(user.getId(), AppToken.getProjectIdLong(), new MethodCallback<List<UserZoneDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
				showUserZones(Collections.emptyList());
			}

			@Override
			public void onSuccess(Method method, List<UserZoneDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				userZones.clear();
				userZones.addAll(result);
				showUserZones(result);
			}
		});
	}
	
	
	private void showUserZones(List<UserZoneDto> userZones) {
		MaterialCollection userZoneCollection = new MaterialCollection();
		
		for (UserZoneDto userZone : userZones) {
			UserZoneView userZoneView = new UserZoneView(userZone);
			userZoneCollection.add(userZoneView);
			userZoneView.setUserDevices(userZone.getUserDevices());
			userZoneView.setUserScenes(userZone.getUserScenes());
			userZoneView.getCheckBoxRole().addValueChangeHandler(new ValueChangeHandler<Boolean>() {
				
				@Override
				public void onValueChange(ValueChangeEvent<Boolean> event) {
					userZone.setRole(userZoneView.getCheckBoxRole().getValue());
					rpcUpdateUserZone(userZone, userZoneView);
				}
			});
			
			addActionHandler(userZoneView.getUserDevices(), userZoneView.getUserScenes(), userZoneView);
		}
		display.getContentPanel().clear();
		display.getContentPanel().add(userZoneCollection);
	}
	
	private void rpcUpdateUserZone(UserZoneDto userZone, UserZoneView userZoneView) {
		rpcProvider.getUserService().updateUserZone(userZone.getId(), userZone, new MethodCallback<UserZoneDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				userZone.setRole(!userZoneView.getCheckBoxRole().getValue());
				userZoneView.getCheckBoxRole().setValue(userZone.getRole());
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, UserZoneDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
				userZoneView.setUserDevices(response.getUserDevices());
				userZoneView.setUserScenes(response.getUserScenes());
				addActionHandler(userZoneView.getUserDevices(), userZoneView.getUserScenes(), userZoneView);
			}
		});
	}
	
	private void addActionHandler(List<UserDeviceView> userDeviceViews, 
			List<UserSceneView> userSceneViews, UserZoneView userZoneView) {
		for (UserDeviceView userDeviceView : userDeviceViews) {
			userDeviceView.getCheckBoxRole().addValueChangeHandler(new ValueChangeHandler<Boolean>() {
				
				@Override
				public void onValueChange(ValueChangeEvent<Boolean> event) {
					userDeviceView.getUserDevice().setRole(userDeviceView.getCheckBoxRole().getValue());
					rpcUpdateUserDevice(userDeviceView.getUserDevice(), userDeviceView, userZoneView);
				}
			});
		}
		for (UserSceneView userSceneView : userSceneViews) {
			userSceneView.getCheckBoxRole().addValueChangeHandler(new ValueChangeHandler<Boolean>() {
				
				@Override
				public void onValueChange(ValueChangeEvent<Boolean> event) {
					userSceneView.getUserScene().setRole(userSceneView.getCheckBoxRole().getValue());
					rpcUpdateUserScene(userSceneView.getUserScene(), userSceneView, userZoneView);
				}
			});
		}
	}

	protected void rpcUpdateUserScene(UserSceneDto userScene, UserSceneView userSceneView, UserZoneView userZoneView) {
		rpcProvider.getUserService().updateUserScene(userScene.getId(), userScene,
				new MethodCallback<UserSceneDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				userScene.setRole(!userSceneView.getCheckBoxRole().getValue());
				userSceneView.getCheckBoxRole().setValue(userScene.getRole());
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, UserSceneDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
				if (response.hasRole() 
						&& Boolean.FALSE.equals(userZoneView.getCheckBoxRole().getValue())) {
					userZoneView.getCheckBoxRole().setValue(true);
				}
			}
		});
	}

	private void rpcUpdateUserDevice(UserDeviceDto userDevice, UserDeviceView userDeviceView, UserZoneView userZoneView) {
		rpcProvider.getUserService().updateUserDevice(userDevice.getId(), userDevice,
				new MethodCallback<UserDeviceDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				userDevice.setRole(!userDeviceView.getCheckBoxRole().getValue());
				userDeviceView.getCheckBoxRole().setValue(userDevice.getRole());
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, UserDeviceDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
						ShowMessageEvent.SUCCESS));
				if (response.hasRole() 
						&& Boolean.FALSE.equals(userZoneView.getCheckBoxRole().getValue())) {
					userZoneView.getCheckBoxRole().setValue(true);
				}
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		//does nothing
	}
}
