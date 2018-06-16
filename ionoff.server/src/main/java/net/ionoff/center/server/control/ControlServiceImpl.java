package net.ionoff.center.server.control;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeScene;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.SceneAction;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.entity.ScenePlayerAction;
import net.ionoff.center.server.entity.SceneRelayAction;
import net.ionoff.center.server.exception.RelayLockedException;
import net.ionoff.center.server.message.event.RelayStatusChangedEvent;
import net.ionoff.center.server.notifier.RelayStatusNotifier;
import net.ionoff.center.server.persistence.dao.IModeDao;
import net.ionoff.center.server.persistence.dao.ISceneDao;
import net.ionoff.center.server.persistence.dao.ISceneDeviceDao;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.relaydriver.api.RelayDriverApiProvider;
import net.ionoff.center.server.relaydriver.api.RelayDriverApiUtil;
import net.ionoff.center.server.relaydriver.api.RelayDriverException;
import net.ionoff.center.server.relaydriver.api.RelayDriverStatus;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.StatusDto;
import net.ionoff.center.shared.entity.PlayerAction;
import net.ionoff.center.shared.entity.RelayAction;
import net.xapxinh.center.server.exception.DataServiceException;
import net.xapxinh.center.server.exception.PlayerConnectException;
import net.xapxinh.center.server.service.player.IPlayerService;
import net.xapxinh.center.shared.dto.Command;
import net.xapxinh.center.shared.dto.PlayerApi;
import net.xapxinh.center.shared.dto.Status;

public class ControlServiceImpl implements IControlService {

	private final Logger logger = Logger.getLogger(ControlServiceImpl.class.getName());
	
	@Autowired
	private IRelayService relayService;
	
	@Autowired
	private IModeDao modeDao;
	
	@Autowired
	private ISceneDao sceneDao;

	@Autowired
	private IPlayerService playerService;
	
	@Autowired
	private ISceneDeviceDao sceneDeviceDao;
	
	@Lazy
	@Autowired
	private RelayStatusNotifier relayStatusNotifier;

	@Autowired
	private RelayDriverApiProvider relayDriverApiProvider;
	
	@Override
	public StatusDto switchOn(long relayId) {
		final Relay relay = relayService.requireById(relayId);
		if (relay.isOpened()) {
			switchRelayToOn(relay);
		}
		return getStatusDto(relay);
	}

	@Override
	public StatusDto switchOff(long relayId) {
		final Relay relay = relayService.requireById(relayId);
		if (relay.isClosed()) {
			switchRelayToOff(relay);
		}
		return getStatusDto(relay);
	}
	
	@Override
	public StatusDto getStatusDto(Relay relay) {
		StatusDto statusDto = new StatusDto();
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
		if (Boolean.TRUE.equals(relay.getIsLocked())) {
			throw new RelayLockedException(relay.getName() + " (" + relay.getDriver().getName() + ")");
		}
		if (state.equals(relay.getStatus())) {
			relayService.update(relay, state);
			return;
		}
		if (!relay.izAutoRevert() || Boolean.FALSE.equals(state)) {
			sendRelayCommand(relay, state);
		}
		else {
			sendRelayCommand(relay, state, relay.getAutoRevert());
			if (relay.izButton()) {								
				relayService.update(relay, false);
			}
		}
	}
	
	private void sendRelayCommand(Relay relay, Boolean state) {
		if (Boolean.TRUE.equals(state)) {
			RelayDriver relayDriver = relay.getDriver();
			relayDriverApiProvider.getRelayDriverApi(relayDriver).closeRelay(relayDriver, relay.getIndex());
			logger.info("Update relay status " + relay.getNameId() + ": true");
			relayService.update(relay, true);
			notifyRelayStateChanged(relay);
		}
		else if (Boolean.FALSE.equals(state)) {
			RelayDriver relayDriver = relay.getDriver();
			relayDriverApiProvider.getRelayDriverApi(relayDriver).openRelay(relayDriver, relay.getIndex());
			logger.info("Update relay status " + relay.getNameId() + ": false");
			relayService.update(relay, false);
			notifyRelayStateChanged(relay); 
		}
	}
	
	private void sendRelayCommand(Relay relay, Boolean state, Integer autoRevert) {
		if (Boolean.TRUE.equals(state)) {
			RelayDriver relayDriver = relay.getDriver();
			relayDriverApiProvider.getRelayDriverApi(relayDriver).closeRelay(relayDriver, relay.getIndex(), autoRevert);
			logger.info("Update relay status " + relay.getNameId() + ": true");
			relayService.update(relay, true);
			notifyRelayStateChanged(relay);
		}
		else if (Boolean.FALSE.equals(state)) {
			RelayDriver relayDriver = relay.getDriver();
			relayDriverApiProvider.getRelayDriverApi(relayDriver).openRelay(relayDriver, relay.getIndex(), autoRevert);
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
			} catch (PlayerConnectException | RelayDriverException
					| DataServiceException | UnknownRelayDriverModelException e) {
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
	public void executePlayerAction(Player player, String action, String volume, String album, String albumType) {
		try {
			if (PlayerAction.PLAY.equalsIgnoreCase(action)) {
				playAlbum(player, volume, album, albumType);
			}
			else if (PlayerAction.STOP.equalsIgnoreCase(action)) {
				stopPlaying(player);
			}
		}
		catch (PlayerConnectException e) {
			logger.error(e.getMessage() + " MAC: " + player.getMac());
		}
	}

	protected void playAlbum(Player player,  String volume, String album, String albumType) 
			throws DataServiceException, PlayerConnectException {
		
		net.xapxinh.center.server.entity.Player p = toPlayer(player);
		Status status = null;
		
		try {
			status = playerService.requesStatus(p, new HashMap<String, Object>());
		}
		catch (final PlayerConnectException e) {
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
	
	private net.xapxinh.center.server.entity.Player toPlayer(Player player) {
		net.xapxinh.center.server.entity.Player p = new net.xapxinh.center.server.entity.Player();
		p.setId(player.getId());
		p.setMac(player.getMac());
		p.setName(player.getName());
		return p;
	}
	
	private void sleep(long time) {
		try {
			Thread.sleep(time);
		} catch (InterruptedException e) {
			logger.error(e.getMessage(), e);
		}
	}

	protected void stopPlaying(Player player) {
		net.xapxinh.center.server.entity.Player p = toPlayer(player);
		Status status = null;
		
		try {
			status = playerService.requesStatus(p, new HashMap<String, Object>());
			if ("playing".equals(status.getState())) {
				logger.info("Player is playing. Stop player now");
				playerService.requesStatus(p, toParamMap(PlayerApi.stopPlaylist()));
			}
		}
		catch (final PlayerConnectException e) {
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
	public RelayDriverStatus getRelayDriverStatus(RelayDriver relayDriver) {
		return relayDriverApiProvider.getRelayDriverApi(relayDriver).getStatus(relayDriver);
	}

	@Override
	public boolean ping(RelayDriver relayDriver) {
		return RelayDriverApiUtil.ping(relayDriver.getIp(), relayDriver.getPort());
	}

	@Override
	public StatusDto turnOnDevice(Device device) {
		StatusDto statusDto = new StatusDto();
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
	public StatusDto turnOffDevice(Device device) {
		StatusDto statusDto = new StatusDto();
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
