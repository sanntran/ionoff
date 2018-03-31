package net.ionoff.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;

import com.google.gson.Gson;

public class ServiceServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;
	private static final Logger LOGGER = Logger.getLogger(ServiceServlet.class.getName());

	private UpdatorThread updatorThread;
	private static final Gson GSON = new Gson();
	
	@Override
	public void init() {
		updatorThread = new UpdatorThread();
		updatorThread.start();
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		String pathInfo = request.getPathInfo();
		if ("/versions/upgrade".equals(pathInfo)) {
			LOGGER.info("Received request to upgrade new version...");
			responseLatestVersion(response);
			String dbVersionDateTime = request.getParameter("currentVerionDateTime");
			updateLatestVersion(dbVersionDateTime);
		}
		if ("/versions/check".equals(pathInfo)) {
			LOGGER.info("Received request to check latest version...");
			Version version = checkLatestVersion();
			responseLatestVersion(response, version);
			updatorThread.downloadLatestVersion();
		}
		else {
			responseLatestVersion(response);
		}
	}

	private Version checkLatestVersion() throws IOException {
		String latestVersionJson = HttpRequestUtil
				.sendHttpGETRequest(UpdatorConfig.getInstance().LATESTVERSION_FILE_URL);
		LOGGER.info("Retrieved latest version from update site: \n" + latestVersionJson);
		Version latestVersion = GSON.fromJson(latestVersionJson, Version.class);
		return latestVersion;
	}

	private void updateLatestVersion(String dbVersionDateTime) throws IOException {
		if (dbVersionDateTime == null) {
			LOGGER.info("DB version date time is not valid");
			return;
		}
		updatorThread.setUpdating(true);
		LOGGER.info("Updating latest version...");
		
		File downloadedVersionFile = new File(UpdatorConfig.getInstance().LOCAL_LATESTVERSION_FILE);
		if (!downloadedVersionFile.exists()) {
			LOGGER.info("No latest version to update");
			return;
		}
		String downloadedVersionFileContent = FileManager.readFile(downloadedVersionFile);
		Version downloadedVersion = new Gson().fromJson(downloadedVersionFileContent, Version.class);
		
		try {
			if (downloadedVersion.getDateTime() != null && downloadedVersion.getDateTime().compareTo(dbVersionDateTime) > 0) {
			
				List<String> latestReleaseFiles = unzipLatestVersion();
				if (latestReleaseFiles.isEmpty()) {
					return;
				}
				LOGGER.info("Undeploying wars...");
				undeployWars();
				LOGGER.info("Run SQL scripts...");
				
				// make sure iserver is undeployed so that it is able to create connection to database hsql
				runUpdateSQLs(dbVersionDateTime, latestReleaseFiles);
				LOGGER.info("Deploy latest wars...");
				deployLatestWars(latestReleaseFiles);
				File updateFolder = new File(UpdatorConfig.getInstance().LOCAL_UPDATE_FOLDER);
				for (File file : updateFolder.listFiles()) {
					if (file.isDirectory()) {
						LOGGER.info("Delete unzipped folder " + file.getName());
						FileManager.deleteFile(file);
					}
				}
				LOGGER.info("Finish upgrade to latest version ");
			}
			else {
				LOGGER.info("The current version is up to date");
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		updatorThread.setUpdating(false);
	}

	private List<String> unzipLatestVersion() throws IOException {
		File updateFolder = new File(UpdatorConfig.getInstance().LOCAL_UPDATE_FOLDER);
		if (!updateFolder.exists()) {
			return Collections.emptyList();
		}
		for (File file : updateFolder.listFiles()) {
			if (file.getName().endsWith(".zip")) {
				LOGGER.info("Unzip " + file.getName());
				return FileManager.unzipFile(file.getAbsolutePath(), UpdatorConfig.getInstance().LOCAL_UPDATE_FOLDER);
			}
		}
		return Collections.emptyList();
	}

	private void deployLatestWars(List<String> latestReleaseFiles) throws IOException {
		for (String releaseFile : latestReleaseFiles) {
			if (releaseFile.endsWith(".war")) {
				File warFile = new File(releaseFile);
				File toWarFile = new File(
						UpdatorConfig.getInstance().WEBAPP_FOLDER + File.separator + warFile.getName());
				LOGGER.info("Moving " + warFile.getName() + " to " + toWarFile.getName());
				warFile.renameTo(toWarFile);
			}
		}
	}

	private void runUpdateSQLs(String dbVersionDateTime, List<String> latestReleaseFiles)
			throws IOException, ClassNotFoundException, SQLException {
		boolean runSQLScript = false;
		for (String releaseFile : latestReleaseFiles) {
			if (releaseFile.endsWith(".sql")) {
				File sqlFile = new File(releaseFile);
				String sqlFileDatetime = sqlFile.getName().replace(".sql", "");
				if (sqlFileDatetime.compareTo(dbVersionDateTime) > 0 ) {
					String sqlScript = FileManager.readFile(sqlFile);
					LOGGER.info("Running SQL script: " + sqlScript);
					runSQLScript = true;
					break;
					
				}
			}
		}
		if (runSQLScript == true) {
			// Create DB Connection
			Class.forName(UpdatorConfig.getInstance().DB_CONNECTION_DRIVER);
			String jdbcUrl = UpdatorConfig.getInstance().DB_CONNECTION_URL;
			String jdbcUser = UpdatorConfig.getInstance().DB_CONNECTION_USER;
			String jdbcPass = UpdatorConfig.getInstance().DB_CONNECTION_PASS;

			Connection con = DriverManager.getConnection(jdbcUrl, jdbcUser, jdbcPass);
			for (String releaseFile : latestReleaseFiles) {
				if (releaseFile.endsWith(".sql")) {
					File sqlFile = new File(releaseFile);
					String sqlFileDatetime = sqlFile.getName().replace(".sql", "");
					if (sqlFileDatetime.compareTo(dbVersionDateTime) > 0 ) {
						String sqlScript = FileManager.readFile(sqlFile);
						LOGGER.info("Running SQL script: " + sqlScript);
						try {
							runSQLScript(con, releaseFile);
						}
						catch (Exception e) {
							LOGGER.error(e.getMessage(), e);
						}
					}
				}
			}
			try {
				con.close();
			}
			catch (Exception e) {
				LOGGER.error(e.getMessage(), e);
			}
		}
	}

	private void runSQLScript(Connection con, String scriptFile) throws SQLException, ClassNotFoundException {
		
		try {
			// Initialize object for ScripRunner
			ScriptRunner scriptRunner = new ScriptRunner(con, false, false);
			// Give the input file to Reader
			Reader reader = new BufferedReader(new FileReader(scriptFile));
			// Exctute script
			scriptRunner.runScript(reader);

		} catch (Exception e) {
			LOGGER.error("Failed to Execute" + scriptFile + " The error is " + e.getMessage());
		}
	}

	private void undeployWars() throws IOException {
		for (String deployedWar : UpdatorConfig.getInstance().DEPLOYED_WARS) {
			undeployWar(deployedWar);
		}
	}

	private void undeployWar(String deployedWar) throws IOException {
		String warFolder = UpdatorConfig.getInstance().WEBAPP_FOLDER + File.separator + deployedWar;
		String warFilePath = warFolder + ".war";
		File warFile = new File(warFilePath);
		if (!warFile.exists()) {
			return;
		}
		LOGGER.info("Undeploying " + deployedWar + ".war ...");
		FileManager.deleteFile(warFile);
		int loop = 0;
		for (; true;) {
			try {
				Thread.sleep(1000);
				File warFolderFile = new File(warFolder);
				if (!warFolderFile.exists()) {
					LOGGER.info("Finish undeploying " + deployedWar + ".war ...");
					return;
				}
				if (loop == 60) { // 1 min
					LOGGER.info("Waited 1 min after undeploying " + deployedWar + ".war. Delete war folder now...");
					FileManager.deleteFile(warFolder);
					break;
				}
				loop++;
			} catch (InterruptedException e) {
				LOGGER.error(e);
			}
		}
	}

	private static void responseLatestVersion(HttpServletResponse response) throws IOException {
		File downloadedVersionFile = new File(UpdatorConfig.getInstance().LOCAL_LATESTVERSION_FILE);
		response.getWriter().append(FileManager.readFile(downloadedVersionFile));
	}
	
	private static void responseLatestVersion(HttpServletResponse response, Version version) throws IOException {
		response.getWriter().append(GSON.toJson(version));
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}
	
	/*
	private Version selectCurrentVersion() {
		Version version = new Version();
		try {
			Class.forName("com.mysql.jdbc.Driver");
			String jdbcUrl = UpdatorConfig.getInstance().DB_CONNECTION_URL;
			String jdbcUser = UpdatorConfig.getInstance().DB_CONNECTION_USER;
			String jdbcPass = UpdatorConfig.getInstance().DB_CONNECTION_PASS;

			Connection conn = DriverManager.getConnection(jdbcUrl, jdbcUser, jdbcPass);
			Statement stmt = conn.createStatement();
			ResultSet rs = stmt.executeQuery("SELECT * FROM versions");
			while (rs.next()) {
				version.setName(rs.getString("name"));
				version.setDateTime(rs.getString("date_time"));
			}
			conn.close();
			return version;
		} catch (Exception e) {
			LOGGER.error("Got an exception!");
			LOGGER.error(e.getMessage());
		}
		return version;
	}
	*/
}
