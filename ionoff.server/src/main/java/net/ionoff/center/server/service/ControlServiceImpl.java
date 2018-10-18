package net.ionoff.center.server.service;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.ionoff.center.server.mediaplayer.exception.MediaPlayerRequestException;
import net.ionoff.center.server.controller.connector.ControllerConnector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeScene;
import net.ionoff.center.server.entity.MediaPlayer;
import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.SceneAction;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.entity.ScenePlayerAction;
import net.ionoff.center.server.entity.SceneRelayAction;
import net.ionoff.center.server.exception.RelayLockedException;
import net.ionoff.center.server.message.RelayStatusNotifier;
import net.ionoff.center.server.message.event.RelayStatusChangedEvent;
import net.ionoff.center.server.persistence.dao.IModeDao;
import net.ionoff.center.server.persistence.dao.IControllerDao;
import net.ionoff.center.server.persistence.dao.ISceneDao;
import net.ionoff.center.server.persistence.dao.ISceneDeviceDao;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.controller.exception.ControllerRequestException;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.entity.PlayerAction;
import net.ionoff.center.shared.entity.RelayAction;
import net.ionoff.center.server.mediadata.exception.MediaDataRequestException;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerConnectException;
import net.ionoff.center.server.mediaplayer.service.IMediaPlayerService;
import net.ionoff.center.shared.dto.player.Command;
import net.ionoff.center.shared.dto.player.PlayerApi;
import net.ionoff.center.shared.dto.player.StatusDto;
import org.springframework.stereotype.Service;

@Service
public class ControlServiceImpl implements IControlService {

	private final Logger logger = LoggerFactory.getLogger(ControlServiceImpl.class.getName());
	
	@Autowired
	private IRelayService relayService;
	
	@Autowired
	private IModeDao modeDao;
	
	@Autowired
	private ISceneDao sceneDao;

	@Autowired
	private IMediaPlayerService playerService;
	
	@Autowired
	private ISceneDeviceDao sceneDeviceDao;
	
	@Lazy
	@Autowired
	private RelayStatusNotifier relayStatusNotifier;

	@Autowired
	private ControllerConnector controllerConnector;
	
	@Autowired
	private IControllerDao controllerDao;
	
	@Override
	public net.ionoff.center.shared.dto.StatusDto switchOn(long relayId) {
		final Relay relay = relayService.requireById(relayId);
		if (relay.isOpened()) {
			switchRelayToOn(relay);
		}
		return getStatusDto(relay);
	}

	@Override
	public net.ionoff.center.shared.dto.StatusDto switchOff(long relayId) {
		final Relay relay = relayService.requireById(relayId);
		if (relay.isClosed()) {
			switchRelayToOff(relay);
		}
		return getStatusDto(relay);
	}
	
	@Override
	public net.ionoff.center.shared.dto.StatusDto getStatusDto(Relay relay) {
		net.ionoff.center.shared.dto.StatusDto statusDto = new net.ionoff.center.shared.dto.StatusDto();
		statusDto.setId(relay.getId());
		statusDto.setValue(relay.getStatus());
		if (relay.getTime() != null) {
			statusDto.setTime(DateTimeUtil.yyyyMMddHHmmFormatter.format(relay.getTime()));
		}
		return statusDto;
	}
	
	@Override
	public void setRelayState(Relay relay, Boolean state) {
		if (state == null) {
			return;
		}
		Long driverId  = relay.getDriver().getId();
		Controller controller = controllerDao.findById(driverId);
		
		if (Boolean.TRUE.equals(relay.getIsLocked())) {
			throw new RelayLockedException(relay.getName() + " (" + controller.getName() + ")");
		}
		if (state.equals(relay.getStatus())) {
			if (!controller.isLazy()) {
				logger.info("Update relay state of relay "
						+ relay.getLabel() + " of " +  controller.getModel() + " to " + state);
				relayService.update(relay, state);
				return;
			}
		}
		if (!relay.izAutoRevert() || Boolean.FALSE.equals(state)) {
			sendRelayCommand(controller, relay, state);
		}
		else {
			sendRelayCommand(controller, relay, state, relay.getAutoRevert());
			if (relay.izButton()) {								
				relayService.update(relay, false);
			}
		}
	}
	
	private void sendRelayCommand(Controller controller, Relay relay, Boolean state) {
		if (Boolean.TRUE.equals(state)) {
			logger.info("Update relay status " + relay.getNameId() + ": true");
			controllerConnector.closeRelay(controller, relay.getIndex());
			relayService.update(relay, true);
			notifyRelayStateChanged(relay);
		}
		else if (Boolean.FALSE.equals(state)) {
			logger.info("Update relay status " + relay.getNameId() + ": false");
			controllerConnector.openRelay(controller, relay.getIndex());
			relayService.update(relay, false);
			notifyRelayStateChanged(relay); 
		}
	}
	
	private void sendRelayCommand(Controller controller, Relay relay, Boolean state, Integer autoRevert) {
		if (Boolean.TRUE.equals(state)) {
			controllerConnector.closeRelay(controller, relay.getIndex(), autoRevert);
			logger.info("Update relay status " + relay.getNameId() + ": true");
			relayService.update(relay, true);
			notifyRelayStateChanged(relay);
		}
		else if (Boolean.FALSE.equals(state)) {
			controllerConnector.openRelay(controller, relay.getIndex(), autoRevert);
			logger.info("Update relay status " + relay.getNameId() + ": false");
			relayService.update(relay, false);
			notifyRelayStateChanged(relay);
		}
	}
	
	@Override
	public void switchRelayToOn(Relay relay) {
		setRelayState(relay, true);
	}

	private void notifyRelayStateChanged(Relay relay) {
		relayStatusNotifier.notifyListeners(new RelayStatusChangedEvent(relay));
	}

	@Override
	public void switchRelayToOff(Relay relay) {
		setRelayState(relay, false);
	}

	@Override
	public void activateMode(Mode mode) {
		if (mode.getScenes() != null) {
			for (final ModeScene modeScene : mode.getScenes()) {
				playScene(modeScene.getScene());
			}
		}
		Project proj = mode.getProject();
		for (Mode m : proj.getModes()) {
			if (m.getIsActivated() == null || m.getIsActivated().booleanValue() == true) {
				m.setIsActivated(false);
				modeDao.update(m);
			}
		}
		mode.setIsActivated(true);
		mode.setTime(new Date());
		modeDao.update(mode);
	}

	private void executeSceneActions(List<SceneAction> sceneActions) {
		if (sceneActions == null) {
			return;
		}
		for (final SceneAction sceneAction : sceneActions) {
			try {
				executeSceneAction(sceneAction);
			} catch (MediaPlayerRequestException | ControllerRequestException
					| MediaDataRequestException e) {
				logger.error("Failed to execute scene action. " + e.getMessage());
			}
		}
	}

	private void executeSceneAction(SceneAction sceneAction) {
		if (sceneAction instanceof ScenePlayerAction) {
			final ScenePlayerAction playerAction = (ScenePlayerAction)sceneAction;
			executePlayerAction(playerAction.getPlayer(), playerAction.getAction(),
					playerAction.getVolume(), playerAction.getAlbum(), playerAction.getAlbumType());
		}
		else if (sceneAction instanceof SceneRelayAction) {
			final SceneRelayAction relayAction = (SceneRelayAction)sceneAction;
			executeRelayAction(relayAction.getRelay(), relayAction.getAction());
		}
		sleep(750);
	}

	@Override
	public void executeRelayAction(Relay relay, String action) {
		if (RelayAction.OPEN.equalsIgnoreCase(action)) {
			switchRelayToOff(relay);
		}
		else if (RelayAction.CLOSE.equalsIgnoreCase(action)) {
			switchRelayToOn(relay);
		}
	}

	@Override
	public void executePlayerAction(MediaPlayer player, String action, String volume, String album, String albumType) {
		try {
			if (PlayerAction.PLAY.equalsIgnoreCase(action)) {
				playAlbum(player, volume, album, albumType);
			}
			else if (PlayerAction.STOP.equalsIgnoreCase(action)) {
				stopPlaying(player);
			}
		}
		catch (MediaPlayerConnectException e) {
			logger.error(e.getMessage() + " MAC: " + player.getMac());
		}
	}

	protected void playAlbum(MediaPlayer player, String volume, String album, String albumType)
			throws MediaDataRequestException, MediaPlayerConnectException {
		
		net.ionoff.center.server.mediaplayer.model.MediaPlayer p = net.ionoff.center.server.mediaplayer.model.MediaPlayer.fromPlayer(player);
		StatusDto status = null;
		
		try {
			status = playerService.requesStatus(p, new HashMap<>());
		}
		catch (final MediaPlayerConnectException e) {
			logger.error(" Failed to play album '" + album + ". Player " + player.getSId() + " is not running.");
			return;
		}
		if ("playing".equals(status.getState())) {
			logger.info("Player is playing. The input album is not played");
			return;
		}
		if (volume != null && isIntNumber(volume)) {
			logger.info("Player volume is now set to " + volume);
			playerService.requesStatus(p, toParamMap(PlayerApi.volumeSet(Integer.parseInt(volume))));
			sleep(1000);
		}

		if (album != null && albumType != null) {
			logger.info("Clear current player playlist... ");
			playerService.requesStatus(p, toParamMap(PlayerApi.emptyPlaylist()));
			sleep(1000);
			logger.info("Play album in player, album name: " + album);
			if (PlayerApi.DIR.equals(albumType)) {
				playerService.requesStatus(p, toParamMap(PlayerApi.addPlayDir(album)));
			}
			else if (PlayerApi.FILE.equals(albumType)) {
				playerService.requesStatus(p, toParamMap(PlayerApi.addPlayFile(album)));
			}
			else {
				logger.error("Unknown album type to play, album type: " + albumType);
			}
		}
	}
	
	private boolean isIntNumber(String num) {
		try {
			Integer.parseInt(num);
		}
		catch (NumberFormatException nfe) {
			return false;
		}
		return true;
	}

	private Map<String, Object> toParamMap(Command command) {
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("command", command.getCommand());
		params.put("id", command.getId());
		params.put("val", command.getVal());
		params.put("input", command.getInput());
		params.put("input_type", command.getInput_type());
		params.put("type", command.getType());
		params.put("title", command.getTitle());
		return params;
	}

	private void sleep(long time) {
		try {
			Thread.sleep(time);
		} catch (InterruptedException e) {
			logger.error(e.getMessage(), e);
		}
	}

	protected void stopPlaying(MediaPlayer player) {
		net.ionoff.center.server.mediaplayer.model.MediaPlayer p = net.ionoff.center.server.mediaplayer.model.MediaPlayer.fromPlayer(player);
		StatusDto status = null;
		
		try {
			status = playerService.requesStatus(p, new HashMap<String, Object>());
			if ("playing".equals(status.getState())) {
				logger.info("Player is playing. Stop player now");
				playerService.requesStatus(p, toParamMap(PlayerApi.stopPlaylist()));
			}
		}
		catch (final MediaPlayerConnectException e) {
			logger.info("Player " + player.getName() + " is not running.");
			return;
		}
	}

	@Override
	public void playScene(Scene scene) {
		if (scene == null || scene.getDevices() == null) {
			logger.info("Trigger playing scene null ...");
			return;
		}
		logger.info("Trigger playing scene " + scene.getNameId() + "...");
		for (final SceneDevice sceneDevice : sceneDeviceDao.findBySceneId(scene.getId())) {
			logger.info("Play scene device " + sceneDevice.getDevice().getName() + " in (second) " + sceneDevice.getDuration());
			long t1 = System.currentTimeMillis(); 
			executeSceneActions(sceneDevice.getActions());
			long t2 = System.currentTimeMillis();
			if (sceneDevice.getDuration() != null) {
				long duration = sceneDevice.getDuration() * 1000;
				if (duration > (t2 - t1)) {
					try {
						long sleep = duration - (t2 - t1);
						logger.info("Sleep to wait for next scene device " + sleep);
						Thread.sleep(sleep);
					} catch (InterruptedException e) {
						logger.error(e.getMessage(), e);
					}
				}
			}
		}
		
		scene.setTime(new Date());
		sceneDao.update(scene);
	}

	@Override
	public net.ionoff.center.shared.dto.StatusDto turnOnDevice(Device device) {
		net.ionoff.center.shared.dto.StatusDto statusDto = new net.ionoff.center.shared.dto.StatusDto();
		statusDto.setId(device.getId());
		if (!device.hasOneRelay()) {
			return statusDto;
		}
		Relay relay = device.getRelayList().get(0);
		switchRelayToOn(relay);
		statusDto.setValue(relay.getStatus());
		statusDto.setTime(DateTimeUtil.yyyyMMddHHmmFormatter.format(relay.getTime()));
		return statusDto;
	}

	@Override
	public net.ionoff.center.shared.dto.StatusDto turnOffDevice(Device device) {
		net.ionoff.center.shared.dto.StatusDto statusDto = new net.ionoff.center.shared.dto.StatusDto();
		if (!device.hasOneRelay()) {
			return statusDto;
		}
		Relay relay = device.getRelayList().get(0);
		switchRelayToOff(relay);
		statusDto.setValue(relay.getStatus());
		statusDto.setTime(DateTimeUtil.yyyyMMddHHmmFormatter.format(relay.getTime()));
		return statusDto;
	}
}
